import fs from 'fs'

import {
  Parser,
  makeFailure,
  succeed,
  fail,
  lazy,
  alt,
  seq,
  seqMap,
  lookahead,
  string as s,
  regex as r,
} from 'parsimmon'

//
// Parser
//

export namespace AST {
  export type Expression = PrimaryExpr | FieldAccessExpr | CallExpr | UnaryExpr
    | BinaryExpr | CompareChainExpr | CondExpr | ArrowFunc

  export type PrimaryExpr = string | Variable | ArrayLiteral | RecordLiteral
  export interface Variable {
    readonly type: 'Variable'
    readonly name: string
  }
  export interface ArrayLiteral {
    readonly type: 'ArrayLiteral'
    readonly exprs: readonly Expression[]
  }
  export interface RecordLiteral {
    readonly type: 'RecordLiteral'
    readonly pairs: ReadonlyArray<{
      readonly key: string
      readonly val: Expression
    }>
  }

  export interface FieldAccessExpr {
    readonly type: 'FieldAccessExpr'
    readonly record: Expression
    readonly fieldName: string
  }
  export interface CallExpr {
    readonly type: 'CallExpr'
    readonly contextArg: Expression | null
    readonly func: Expression
    readonly args: ReadonlyArray<{
      readonly label: string | null
      readonly arg: Expression
    }>
  }
  export interface UnaryExpr {
    readonly type: 'UnaryExpr'
    readonly op: '-' | '!'
    readonly arg: Expression
  }
  export interface BinaryExpr {
    readonly type: 'BinaryExpr'
    readonly op: '**' | '*' | '/' | '%' | '+' | '-' | '!=' | '==' | '<' | '>' | '<='
      | '>=' | '&&' | '||' // in order of precedence
    readonly left: Expression
    readonly right: Expression
  }
  export interface CompareChainExpr {
    readonly type: 'CompareChainExpr'
    readonly chain: readonly BinaryExpr[]
  }
  export interface CondExpr {
    readonly type: 'CondExpr'
    readonly test: Expression
    readonly ifYes: Expression
    readonly ifNo: Expression
  }
  export interface ArrowFunc {
    readonly type: 'ArrowFunc'
    readonly params: readonly string[]
    readonly body: Expression | readonly Statement[]
  }


  export type Statement = ReturnStmt | EmitStmt | LetStmt | ChangeStmt
    | DoStmt | GetDoStmt | FutureDoStmt | AfterGotStmt

  export interface ReturnStmt {
    readonly type: 'ReturnStmt'
    readonly expr: Expression
  }
  export interface EmitStmt {
    readonly type: 'EmitStmt'
    readonly expr: Expression
  }

  export interface LetStmt {
    readonly type: 'LetStmt'
    readonly varName: string
    readonly expr: Expression
  }
  export interface ChangeStmt {
    readonly type: 'ChangeStmt'
    readonly varName: string
    readonly expr: Expression
  }

  export interface DoStmt {
    readonly type: 'DoStmt'
    readonly expr: Expression
  }
  export interface GetDoStmt {
    readonly type: 'GetDoStmt'
    readonly varName: string
    readonly expr: Expression
  }
  export interface FutureDoStmt {
    readonly type: 'FutureDoStmt'
    readonly varName: string
    readonly expr: Expression
  }
  export interface AfterGotStmt {
    readonly type: 'AfterGotStmt'
    readonly vars: readonly string[]
  }


  export interface StateDecl {
    readonly type: 'StateDecl'
    readonly varName: string
    readonly expr: Expression
  }
  export interface WhenDecl {
    readonly type: 'WhenDecl'
    readonly event: Expression
    readonly varName: string | null
    readonly body: readonly Statement[]
  }

  export type TopLevel = StateDecl | WhenDecl | DoStmt | GetDoStmt
}

// who says you can't do left-recursion in a top-down parser? Come at me!
function leftRecur<Op, BinExpr>(
  argParser: Parser<BinExpr>,
  opParser: Parser<Op>,
  map: (op: Op, left: BinExpr, right: BinExpr) => BinExpr
): Parser<BinExpr> {
  return seqMap(argParser, seq(opParser, argParser).many(), (first, rest) =>
    rest.reduce((left, [op, right]) => map(op, left, right), first as BinExpr))
}

// many <parser>'s separated by <separator>, with optional trailing <separator>
//   Usage: <parser>.thru(sepByOptTrailing(<separator>))
function sepByOptTrailing<R>(separator: Parser<unknown>) {
  return (parser: Parser<R>) => seqMap(
    parser.skip(separator).many(),
    parser.map(r => [r]).or(succeed([])),
    (results, result) => results.concat(result)
  )
}

// (some of the) Lexical Grammar

// Whitespace
const _ = r(/[ \n]*/)  // optional whitespace
const __ = r(/[ \n]+/) // required whitespace
const _nonNL = r(/ */) // whitespace, no newlines
const _EOL = r(/ *(?:\/\/[^\n]*)?\n/).desc('end-of-line') // end-of-line,
  // including optional whitespace and line comment

// Note that Tabs are banned, as are exotic whitespace \a\b\v\f\r, except
// in string literals. Just think of banned whitespace like control
// characters and non-printing characters.

// By convention, rules all start and end on non-whitespace, and expect
// the parent rule to deal with surrounding whitespace, EXCEPT that
// StatementIndentBlock expects leading indentation (so it must only be
// invoked immediately after a newline)


const Identifier = r(/[a-z](?:[a-z0-9]|_[a-z0-9])*/i) // TODO non-English letters etc
  .desc('identifier (e.g. example_identifier)')


export function parserAtIndent(indent: string): {
  Expression: Parser<AST.Expression>,
  Statement: Parser<AST.Statement>,
  StatementIndentBlock: Parser<AST.Statement[]>,
  DoStmt: Parser<AST.DoStmt>,
  GetDoStmt: Parser<AST.GetDoStmt>,
} {
  //
  // Expression Grammar (based on JS)
  //   https://tc39.es/ecma262/#sec-ecmascript-language-expressions
  //
  const Expression: Parser<AST.Expression> = lazy(() => alt(ArrowFunc, CondExpr))

  const ParenGroup = s('(').then(Expression.trim(_)).skip(s(')'))
  const Variable = Identifier.map(name => ({ type: 'Variable', name } as const))
  const Numeral = r(/\d(?:\d|_\d)*/).desc('numeral (e.g. 123, 9_000)') // TODO decimals, exponential notation
  const FieldFunc = s('.').then(Identifier).map(name => '.' + name)
    .desc('field access function (e.g. .field_name)')
  const StringLiteral = r(/"(?:\\"|[^"])*"|'(?:\\'|[^'])*'/)
    .desc(`string literal (e.g. "..." or '...')`)
    .chain(str => {
      let i = 0, errOffset = 0, errMsg = ''
      const dedented = str.replace(/\n */g, prefix => {
        i += 1
        if (prefix.length - 1 >= indent.length) {
          return '\n' + prefix.slice(1 + indent.length)
        } else {
          errOffset = str.split('\n').slice(0, i).join('\n').length
            + prefix.length
          errMsg = `Multiline string improperly indented\n`
            + `Only indented ${prefix.length - 1} spaces, needs to be `
            + `indented ${indent.length} spaces`
          return ''
        }
      })
      if (errOffset) return Parser(() => makeFailure(errOffset, errMsg))
      return succeed(dedented)
    })
  const ArrayLiteral = s('[').then(
    Expression.thru(sepByOptTrailing(s(',').trim(_))).trim(_)
    .map(exprs => ({ type: 'ArrayLiteral', exprs } as const))
  ).skip(s(']'))
    .desc('array literal (e.g. [ ... ])')
  const RecordLiteral = s('{').then(
    seqMap(Identifier, alt(s(':').trim(_).then(Expression), succeed(null)),
      (key, val) => ({ key, val: val ?? { type: 'Variable', name: key } }))
    .thru(sepByOptTrailing(s(',').trim(_))).trim(_)
    .map(pairs => ({ type: 'RecordLiteral', pairs } as const))
  ).skip(s('}'))
    .desc('record literal (e.g. { ... })')

  const PrimaryExpr = alt( // in terms of operator precedence, "primary expressions"
      // are the smallest units, which are either actual leaf nodes (variables,
      // numberals, string literals) or delimited groups (parenthesized exprs,
      // array literals, record literals, function calls, anonymous functions).
      // They are the operands to the tightest-binding operator
    ParenGroup,
    Variable,
    Numeral,
    FieldFunc,
    StringLiteral,
    ArrayLiteral,
    RecordLiteral,
  )
  const FieldAccessExpr = alt( // (tightest-binding operator)
    seqMap(PrimaryExpr.skip(s('.').trim(_)), Identifier, (record, fieldName) =>
      ({ type: 'FieldAccessExpr', record, fieldName } as const)),
    PrimaryExpr,
  )
  const CallExpr = alt( // as an exception to operator precedence, binds tighter
    seqMap( // leftwards than FieldAccessExpr, because of how '.' is overloaded
      PrimaryExpr.skip(_),
      s('.').then(Identifier.trim(_)).or(succeed(null)),
      s('(').then(
        seqMap(Identifier.skip(s(':').trim(_)).or(succeed(null)), Expression,
          (label, arg) => ({ label, arg })).sepBy(s(',').trim(_)).trim(_)
      ).skip(s(')')),
      (expr, infixIdent, args) => ({
        type: 'CallExpr',
        contextArg: (infixIdent === null ? null : expr),
        func:       (infixIdent === null ? expr
                      : { type: 'Variable', name: infixIdent } as const),
        args,
      } as const)
    ),
    FieldAccessExpr,
  )
  const UnaryExpr = alt(
    seqMap(r(/[-!]/).skip(_) as Parser<'-' | '!'>, CallExpr,
      (op, arg) => ({ type: 'UnaryExpr', op, arg } as const)),
    CallExpr,
  )
  const ExponentExpr = alt(
    seqMap(CallExpr.skip(s('**').trim(_)), UnaryExpr,
      (left, right) => ({ type: 'BinaryExpr', op: '**', left, right } as const)),
        // Note 1: as a special exception to typical operator precedence,
        // exponents binds equally tightly leftwards as UnaryExpr, to avoid the
        // ambiguity of whether -2**2 is (-2)**2 = 4 or -(2**2) = -4
        //   https://tc39.es/ecma262/#sec-exp-operator
        // In Chrome, -2**2 throws:
        //   Uncaught SyntaxError: Unary operator used immediately before
        //   exponentiation expression. Parenthesis must be used to disambiguate
        //   operator precedence
        //
        // Note 2: in a breaking change from JS, Mechanical also prohibits
        // chaining of exponents because it's not associative, i.e. it's
        // ambiguous whether 2**3**2 is (2**3)**2 = 64 or 2**(3**2) = 512.
        // In JS it's right-associative (so the answer is 512), which I think
        // is confusing because the other non-associative operators are
        // left-associative, e.g. 2/3/4 = (2/3)/4 and not 2/(3/4)
    UnaryExpr,
  )
  const MultExpr = leftRecur(
    ExponentExpr, r(/[*/%]/).trim(_) as Parser<'*' | '/' | '%'>,
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right } as const)
  )
  const AddExpr = leftRecur(
    MultExpr, r(/[+-]/).trim(_) as Parser<'+' | '-'>,
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right } as const)
  )
  const InequalityExpr = seqMap( // mutually
      // exclusive precedence with other comparisons and cannot be chained,
      // because (1 != 2 != 1) == #yes could be surprising, but anything else
      // would require quadratic comparisons
    AddExpr.skip(s('!=').trim(_)), AddExpr,
    (left, right) => ({ type: 'BinaryExpr', op: '!=', left, right } as const)
  )
  const CompareChainExpr: Parser<AST.Expression> = seqMap(
    AddExpr,
    seqMap(
      seq(s<string>('==').trim(_), AddExpr).many(), // we need to special-case
        // the leading =='s because alt() requires failure to fall-through to the
        // next alternative, and when parsing "a == b > c" the first alternative
        // won't fail, rather it succeeds parsing '== b', but the overall parser
        // then fails on '>'
      alt(
        seq(r(/<=?|==/).trim(_), AddExpr).atLeast(1),
        seq(r(/>=?|==/).trim(_), AddExpr).atLeast(1),
        succeed([]),
      ),
      (undirected, directed) => undirected.concat(directed)
    ),
    (first, rest) => rest.length == 0
      ? first
      : rest.length === 1
        ? {
            type: 'BinaryExpr',
            op: rest[0][0] as AST.BinaryExpr['op'],
            left: first,
            right: rest[0][1],
          } as const
        : ({
            type: 'CompareChainExpr',
            chain: rest.map(([op, arg], i) => ({
              type: 'BinaryExpr',
              op: op as AST.BinaryExpr['op'],
              left: i ? rest[i-1][1] : first,
              right: arg,
            } as const)),
          })
  )
  const CompareExpr = alt(InequalityExpr, CompareChainExpr)
  const AndExpr = leftRecur( // conventionally higher precedence than OrExpr
    CompareExpr, s('&&').trim(_),
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right } as const)
  )
  const OrExpr = leftRecur( // conventionally lower precedence than AndExpr
    AndExpr, s('||').trim(_),
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right } as const)
  )
  const CondExpr = alt(
    seqMap(
      OrExpr.skip(s('?').trim(_)), Expression.skip(s(':').trim(_)), Expression,
      (test, ifYes, ifNo) => ({ type: 'CondExpr', test, ifYes, ifNo } as const)
    ),
    OrExpr,
  )
  const ArrowFunc = lazy<AST.ArrowFunc>(() => seqMap(
    alt(
      Identifier.map(param => [param]),
      s('(').then(Identifier.sepBy1(s(',').trim(_)).trim(_)).skip(s(')')),
    )
    .skip(s('=>').trim(_)),
    alt<AST.Expression | AST.Statement[]>(
      lookahead(/[^{]/).then(Expression), // negative lookahead to prohibit
        // record literals, same as JS, even though our statement syntax is
        // actually restrictive enough that it's not ambiguous, unlike JS.
        // It's still nice to reduce visual ambiguity, i.e. syntax that
        // visually looks similar and you would have to look closely to
        // disambiguate. Same reasoning for indentation-sensitivity
        // TODO: reconsider this, we could just let syntax highlighting
        //       ameliorate any visual ambiguity
      StatementBraceBlock,
    ),
    (params, body) => ({ type: 'ArrowFunc', params, body } as const)
  ))


  //
  // Statements
  // allowed in the body of an event handler declaration, or in a cmd {} block
  //
  const ReturnStmt = s('Return').then(__).then(Expression)
    .map(expr => ({ type: 'ReturnStmt', expr } as const))
  const EmitStmt = s('Emit').then(__).then(Expression)
    .map(expr => ({ type: 'EmitStmt', expr } as const))

  const LetStmt = seqMap(
    s('Let').then(__).then(Identifier), s('=').trim(_).then(Expression),
    (varName, expr) => ({ type: 'LetStmt', varName, expr } as const),
  )
  const ChangeStmt = seqMap(
    s('Change').then(__).then(Identifier), s('to').trim(_).then(Expression),
    (varName, expr) => ({ type: 'ChangeStmt', varName, expr } as const)
  )

  const DoStmt = s('Do').then(__).then(Expression)
    .map<AST.DoStmt>(expr => ({ type: 'DoStmt', expr } as const))
  const GetDoStmt = seqMap(
    s('Get').then(__).then(Identifier),
    s('=').trim(_).then(s('Do')).then(_).then(Expression),
    (varName, expr) => ({ type: 'GetDoStmt', varName, expr } as const)
  )
  const FutureDoStmt = seqMap(
    s('Future').then(__).then(Identifier),
    s('=').trim(_).then(s('Do')).then(_).then(Expression),
    (varName, expr) => ({ type: 'FutureDoStmt', varName, expr } as const)
  )
  const AfterGotStmt = r(/~* *After +got +/).then(
    Identifier.sepBy1(s(',').trim(_))
    .desc('at least one variable required in "After got ..." statement')
  ).skip(r(/ *~*/)).map(vars => ({ type: 'AfterGotStmt', vars } as const))

  const Statement = alt<AST.Statement>(ReturnStmt, EmitStmt, LetStmt,
    ChangeStmt, DoStmt, GetDoStmt, FutureDoStmt, AfterGotStmt)

  const StatementIndentBlock = _EOL.many().then(_nonNL).chain(newIndent => {
    if (newIndent.length > indent.length) {
      const { Statement } = parserAtIndent(newIndent)
      return Statement.sepBy1(_EOL.atLeast(1).then(_nonNL).chain(nextIndent => {
        if (nextIndent.length === newIndent.length) return succeed('')
        return fail(`Statement improperly indented\n`
          + `Indented ${nextIndent.length} spaces, needs to be indented `
          + `exactly ${newIndent.length} spaces`)
        // XXX TODO: if the line clearly does parse correctly as a statement,
        //           forgiving parser should clearly explain that indentation
        //           is the reason for error (currently, indentation is listed
        //           as one of many possible reasons, because an expression
        //           continuation is also possible, as in '1\n+ 2', so it's
        //           like, expected proper indent *or* '+' or '-' etc).
        //           Forgiving parser-based error message isn't the only
        //           solution, line continuations should probably be indented
      })).map(stmts => stmts.filter(Boolean))
    }
    return fail(`Statement block improperly indented\n`
      + `Only indented ${newIndent.length} spaces, needs to be indented `
      + `>${indent.length} spaces`)
  })

  const StatementBraceBlock = s('{').then(
    alt(
      Statement.sepBy1(s(';').trim(_nonNL)).trim(_nonNL),
      StatementIndentBlock.trim(_EOL).skip(s(indent)),
    )
  ).skip(s('}'))

  return { Expression, Statement, StatementIndentBlock, DoStmt, GetDoStmt }
}

export const parser = parserAtIndent('')


//
// Top-Level Declarations
// all exported
//
const StateDecl = seqMap(
  s('State').then(__).then(Identifier), s('=').trim(_).then(parser.Expression),
  (varName, expr) => ({ type: 'StateDecl', varName, expr } as const)
)
const WhenDecl = seqMap(
  s('When').then(__).then(parser.Expression),
  alt(s('with').trim(__).then(Identifier), succeed(null))
  .skip(_).skip(s(':')).skip(_EOL),
  parser.StatementIndentBlock,
  (event, varName, body) => ({ type: 'WhenDecl', event, varName, body } as const)
)
export const TopLevel =
  alt<AST.TopLevel>(StateDecl, WhenDecl, parser.DoStmt, parser.GetDoStmt)


export const ProgramParser = s('Mechanical v0.0.1\n').then(
  alt(TopLevel, _nonNL.result(null))
  .sepBy(_EOL.desc('Top-level declarations cannot be indented'))
).map((decls => decls.filter(Boolean)) as <T>(decl: T[]) => Exclude<T, null>[])




//
// Type System
//
export namespace Types {
  // Anything and Nothing are the top and bottom types, aka the universal and empty types
  export type Type = NontrivialType | 'Anything' | 'Nothing'
  export type NontrivialType = PrimitiveType | CompositeType | Func

  // None is the unit type
  export type PrimitiveType = 'None' | Err | ScalarType
  export type ScalarType = bool | num | str

  export type CompositeType = Arr | Product | Sum // | SumOfProducts

  export interface Err {
    readonly species: 'Error'
    readonly error: Sum
    readonly nullable?: true
  }
  interface Errorable {
    readonly nullable?: true
    readonly error?: Sum
  }

  // scalars
  export interface bool extends Errorable { readonly species: 'boolean' }
  export interface num  extends Errorable { readonly species: 'number'  }
  export interface str  extends Errorable { readonly species: 'string'  }

  // array/list/vector
  export interface Arr extends Errorable {
    readonly species: 'Array'
    readonly ItemType: NontrivialType
  }

  // simple product type, aka a record
  export interface Product extends Errorable {
    readonly species: 'Product'
    readonly fields: { readonly [name: string]: NontrivialType }
  }

  // sum type, aka hashtagged union, aka tagged union, aka discriminated union,
  //           aka disjoint union, aka variant type, aka coproduct
  export interface Sum extends Errorable {
    readonly species: 'Sum'
    readonly variants: { readonly [tag: string]: NontrivialType | null }
    //readonly opaque?: true // TODO: implement
  }

  // TODO: not implemented yet
  //
  // sum-of-products, disjoint union of product types
  //                  (i.e. a union of disjoint record types)
  //
  // Recursive, tree-shaped.
  // For example, consider a function that returns:
  //   { x: #simple, foo: 1}
  //   { x: #complex, y: #a, z: #a, bar: 1 }
  //   { x: #complex, y: #a, z: #b, baz: 1 }
  //   { x: #complex, y: #b, z: #a, qux: 1 }
  //   { x: #complex, y: #b, z: #b, zot: 1 }
  //
  // Then the inferred sum-of-products type will be:
  //   {
  //     species: 'SumOfProducts',
  //     discriminant: 'x',
  //     variants: {
  //       'simple': {
  //         species: 'Product',
  //         fields: {
  //           'x':   { species: 'Sum', variants: { 'simple': null } },
  //           'foo': { species: 'number' },
  //         },
  //       },
  //       'complex': {
  //         species: 'SumOfProduct',
  //         discriminant: 'y',
  //         variants: {
  //           'a' : {
  //             species: 'SumOfProduct',
  //             discriminant: 'z',
  //             variants: {
  //               'a': {
  //                 species: 'Product',
  //                 fields: {
  //                   'x':   { species: 'Sum', variants: { 'complex': null } },
  //                   'y':   { species: 'Sum', variants: { 'a':       null } },
  //                   'z':   { species: 'Sum', variants: { 'a':       null } },
  //                   'bar': { species: 'number' },
  //                 },
  //               },
  //               'b': {
  //                 species: 'Product',
  //                 fields: {
  //                   'x':   { species: 'Sum', variants: { 'complex': null } },
  //                   'y':   { species: 'Sum', variants: { 'a':       null } },
  //                   'z':   { species: 'Sum', variants: { 'b':       null } },
  //                   'baz': { species: 'number' },
  //                 },
  //               },
  //             },
  //           },
  //           'b' : {
  //             species: 'SumOfProduct',
  //             discriminant: 'z',
  //             variants: {
  //               'a': {
  //                 species: 'Product',
  //                 fields: {
  //                   'x':   { species: 'Sum', variants: { 'complex': null } },
  //                   'y':   { species: 'Sum', variants: { 'b':       null } },
  //                   'z':   { species: 'Sum', variants: { 'a':       null } },
  //                   'qux': { species: 'number' },
  //                 },
  //               },
  //               'b': {
  //                 species: 'Product',
  //                 fields: {
  //                   'x':   { species: 'Sum', variants: { 'complex': null } },
  //                   'y':   { species: 'Sum', variants: { 'b':       null } },
  //                   'z':   { species: 'Sum', variants: { 'b':       null } },
  //                   'zot': { species: 'number' },
  //                 },
  //               },
  //             },
  //           },
  //         },
  //       },
  //     },
  //   }
  // Note that while 'x' must necessarily be at the highest level, the order
  // of 'y' and 'z' is arbitrarily enforced to be lexicographic.
  // The issue of having to arbitrarily choose the order of discriminant fields
  // can only occur when, at a particular level, multiple fields are shared by
  // every record subtype.
  //
  // What if no one field is shared by every record subtype? Consider:
  //   { x: #a, y: #a, foo: 1 }
  //   { x: #b, z: #a, bar: 1 }
  //   { y: #b, z: #b, qux: 1 }
  //
  // The fields are sorted by which are shared by the most record subtypes,
  // then lexicographically:
  //   {
  //     species: 'SumOfProducts',
  //     discriminant: 'x',
  //     variants: {
  //       'a': {
  //         species: 'Product',
  //         fields: {
  //           'x':   { species: 'Sum', variants: { 'a': null }},
  //           'y':   { species: 'Sum', variants: { 'a': null }},
  //           'foo': { species: 'number' },
  //         },
  //       },
  //       'b': {
  //         species: 'Product',
  //         fields: {
  //           'x':   { species: 'Sum', variants: { 'b': null }},
  //           'z':   { species: 'Sum', variants: { 'a': null }},
  //           'bar': { species: 'number' },
  //         },
  //       },
  //     },
  //     others: [
  //       {
  //         species: 'Product',
  //         fields: {
  //           'y':   { species: 'Sum', variants: { 'b': null }},
  //           'z':   { species: 'Sum', variants: { 'b': null }},
  //           'qux': { species: 'number' },
  //         },
  //       }
  //     ],
  //   }
  //
  // When determining whether another type is disjoint, it will be necessary
  // to compare with both the relevant variant subtype and all the subtypes
  // in `others`. Consider:
  //   { x: #a, y: #b, z: #b, zig: 1 }
  //
  // If you only compare that with the `x: #a` variant ({ x: #a, y: #a, foo: 1 }),
  // it would appear disjoint, but it actually overlaps with the subtype in
  // `others`, { y: #b, z: #b, qux: 1 }. This is a problem because there's
  // nothing you can pattern-match on to distinguish between the two types--in
  // fact, { x: #a, y: #b, z: #b, qux: 1, zig: 1 } is a subtype of both, so
  // which branch would a pattern-match take?
  //
  // Or else if there is no relevant variant subtype, it will be necessary to
  // compare with every subtype. If you only compare this:
  //   { y: #a, z: #b, zag: 1 }
  //
  // with the subtype in `others` ({ y: #b, z: #b, qux: 1 }), it would appear
  // disjoint, but actually overlaps with { x: #a, y: #a, foo: 1 }, and
  // { x: #a, y: #a, z: #b, foo: 1, zag: 1 } is a subtype of both.
  interface SumOfProducts extends Errorable {
    readonly species: 'SumOfProducts'
    readonly discriminant: string // name of the field with discriminating tag
    readonly variants: { readonly [tag: string]: SumOfProducts | Product }
    readonly others?: Array<SumOfProducts | Product> // records without the field
  }

  // monomorphic for now. TODO: parametric polymorphism (Hindley-Milner)
  export type Func = FuncUnary | FuncBinary | FuncNary
  interface AbstractFunc extends Errorable {
    readonly species: 'Function'
    readonly result: NontrivialType
  }
  export interface FuncUnary extends AbstractFunc {
    readonly paramCount: 1
    readonly param: NontrivialType
  }
  export interface FuncBinary extends AbstractFunc {
    readonly paramCount: 2
    readonly param1: NontrivialType
    readonly param2: NontrivialType
    readonly param2Optional?: true
  }
  export interface FuncNary extends AbstractFunc {
    readonly paramCount: 99 // there's no Infinity literal type https://git.io/JT835
    readonly firstParam: NontrivialType
    readonly labels: readonly string[]
    readonly params: { readonly [label: string]: NontrivialType }
    readonly optionalCount: number
  }

  export function isSubtype(T: Type, U: Type): boolean {
    if (T === 'Nothing'  || U === 'Anything') return true
    if (T === 'Anything' || U === 'Nothing')  return false

    if (U === 'None') return T === 'None'
    if (T === 'None') return !!U.nullable

    if ((T.nullable && !U.nullable)
      || (T.error && (!U.error || !isSubtype(T.error, U.error)))
    ) return false

    if (T.species === 'Error') return true

    if (U.species !== T.species) return false

    switch (T.species) {
      case 'boolean':
      case 'number':
      case 'string':
        return true
      case 'Array':
        return U.species === 'Array' && isSubtype(T.ItemType, U.ItemType)
      case 'Product':
        return U.species === 'Product'
          && Object.entries(U.fields).every(([fieldName, U_fieldType]) =>
            fieldName in T.fields
            && isSubtype(T.fields[fieldName], U_fieldType))
      case 'Sum':
        return U.species === 'Sum'
          && Object.entries(T.variants).every(([tag, T_variant]) => {
            if (!(tag in U.variants)) return
            const U_variant = U.variants[tag]
            return T_variant && U_variant ? isSubtype(T_variant, U_variant)
              : T_variant === U_variant
          })
      case 'Function':
        // In designing how functions work, there are some design
        // choices that, once committed to, can't be changed without
        // breaking backwards compatibility. So instead I punted on
        // them and enforce redundant restrictions to ensure
        // forwards-compatibility with either choice.
        //
        // As currently implemented:
        //   - Parameter labels must be unique.
        //   - Labeled parameter order is enforced.
        //   - Optional parameters are only allowed at the end of the
        //     parameter list, and can only be omitted from the end, not
        //     in the middle (so for (a?, b?, c?), can call with (a) or
        //     (a, b) but can't call with (a, c)).
        //   - Function subtyping cannot discard arguments, i.e. you
        //     cannot call a unary function with 2 arguments.
        //     (However, if a binary function's second parameter is
        //      optional, then you can call it with only one argument,
        //      and it can be subtype of a unary functions, if parameter
        //      and return types are compatible.)
        //
        // These are deliberately kinda redundant: if parameter labels
        // must be unique and function subtyping cannot discard arguments,
        // why should parameter order matter?
        //
        // I'd like to remove some of these restrictions, but we can't
        // easily remove all of them at once because they're somewhat
        // mutually exclusive: if parameter order doesn't matter, and
        // you call a binary function with 4 arguments, which of the
        // unordered labeled arguments becomes the second argument?
        // If parameter order doesn't matter, and a function has duplicate
        // labels for parameters with different but overlapping product
        // types, can row-polymorphism change the order that arguments
        // get mapped to parameters?
        //
        // Some kind of middle ground might be possible, like "static"
        // functions can be called with labeled parameters in any order,
        // whereas "dynamic" functions must be called with a consistent
        // parameter ordering. Function values passed in via a parameter
        // or from state are obviously dynamic; what about return values
        // from higher-order functions?
        //
        // But if function parameters have implicit order (even though
        // they can sometimes be supplied out-of-order), that makes it
        // hard to allow optional params to be omitted not at the end:
        //   (string, a?: string, b?: string) => string
        //   (string, b?: string, a?: string) => string
        // (Imagine a function checking a conditional and returning one
        // of two functions.) The resulting function can be called with
        // either signature (string, a: string) or (string, b: string),
        // but what would be the implicit order if we allowed both the
        // `a` and `b`-labeled parameters?
        //
        // So the main mutually-exclusive things are:
        //   - subtyping by discarding arguments requires at least
        //     implicit order; that makes it hard to allow optional
        //     parameters to be omitted out-of-order
        //   - duplicate labels either have to have enforced order, or
        //     or have to have type restrictions (they might even have
        //     to have the same type, I'm not sure disjoint types can
        //     even work considering unioning)
        //
        // Which means the 4 options are:
        //   - explicit or implicit enforced order: allow subtyping by
        //     discarding, and enforce order on duplicate labels, at the
        //     price of out-of-order optionals or duplicate labels
        //   - very implicit order: allow subtyping by discarding, and
        //     allow out-of-order params including duplicate labels, at
        //     the price of tricky type restrictions on duplicate labels
        //     and tricky semantics of out-of-order optionals
        //     (lexicographic priority?)
        //   - unordered except for duplicate labels: labeled params can
        //     be in any order, and optionals omitted in any order, except
        //     duplicate labels must be in order and if optional can only
        //     be omitted from the end. No subtyping by discarding
        //   - totally unordered: labeled params can be in any order, and
        //     optionals omitted in any order, even duplicates, at the
        //     price of requiring duplicate labels to be the same type
        //     (maybe disjoint can work too, not sure) and no subtyping
        //     by discarding
        //
        // The two philosophies, I think, are Python named parameters-style,
        // which points towards unordered (and unique) named params; and
        // the Smalltalk/Swift-style, where labeled parameters are like an
        // English phrase, which points towards enforcing some order and
        // allowing free reign to duplicate labels of any types.
        //
        // I'm conflicted because I prefer the English-phrase-style, but
        // I also want to omit optionals in any order. So I'm currently
        // leaning towards "unordered except for duplicate labels",
        // because I don't see much use for the subtyping-by-discarding.
        //
        // Another issue (haven't figured out how to it fits into the axes
        // above) is the (current) distinction between binary functions
        // that take two unlabeled parameters, and functions that take one
        // labeled parameter (and the always-unlabeled first parameter).
        // The latter has to be possible because you can get it by unioning
        // functions, but then it could be ambiguous if it could be a
        // property value shorthand:
        //
        //     Let foo = (thing, x: first_arg, y?: second_arg) => ...
        //     Let bar = (thing, x: first_arg, z?: second_arg) => ...
        //     Let f = predicate ? foo : bar
        //
        //     // unambiguous:
        //     thing.f(x: 1)
        //
        //     // should work:
        //     Let x = 1
        //     thing.f(x)
        //
        //     // ambiguous:
        //     Let qux = (f, t, x) => t.f(x)
        //     qux(f)
        //     //  ^ is f inferred to be a binary function, or a function
        //     //    with one labeled parameter, label `x`?
        //
        // I think we could solve this, and still support "unordered except
        // for duplicate labels", if the second argument in a binary function
        // isn't tied to a label:
        //
        //     Let foo = (x, a?, b?) => { x, a, b }
        //     Let bar = (x, b?, a?) => { x, a, b }
        //     Let f = predicate ? foo : bar
        //
        // Then `1.f(2)` can return either { x: 1, a: 2, b: #none } or
        // { x: 1, a: #none, b: 2 } depending on `predicate`.
        // This way, the function types can be unioned without regard for
        // parameter order, and any n-ary function with enough optional
        // arguments can be a binary function.
        // I think this should work, because any given function *value*
        // must intrinsically have an obvious second parameter.
        // There's some weirdness because function types will have to
        // include a parameter type for the second-argument-if-called-as-
        // a-binary-function; I'm thinking a semicolon would separate the
        // unlabeled and labeled parameters, so:
        //     (string; a: string, b: string) => string
        //       ^ must be called like: f("x", a: "y", "z")
        //     (string, string; a?: string, b?: string) => string
        //       ^ may be called like any of: f("x", a: "y", z: "y")
        //                                or: f("x", a: "y")
        //                                or: f("x", b: "y")
        //                                or: f("x", "y")
        //         Note that if f is a union of foo and bar like above, then
        //         the first 3 calls will return:
        //             { x: "x", a: "y",   b: "z"   }
        //             { x: "x", a: "y",   b: #none }
        //             { x: "x", a: #none, b: "y"   }
        //         Respectively, regardless of `predicate`. Whereas the last
        //         call, the binary function call, will return:
        //             { x: "x", a: "y", b: #none }
        //             OR
        //             { x: "x", a: #none, b: ""}
        //         Depending on `predicate`.
        //
        // This might make codegen more complex? But maybe not compared
        // to what it'll take to support unordered params anyway.
        //
        // TODO: open a discussion ticket
        if (U.species !== 'Function') return false
        if (T.paramCount === 1) {
          if (U.paramCount !== 1) return false
          return isSubtype(U.param,  T.param)  // contravariant
            &&   isSubtype(T.result, U.result) // covariant
        }
        if (T.paramCount === 2) {
          if (U.paramCount === 99) return false
          if (U.paramCount === 1 && !T.param2Optional) return false
          if (U.paramCount === 2 && U.param2Optional
            && !T.param2Optional) return false
          const U_param1 = U.paramCount === 1 ? U.param   : U.param1
          const U_param2 = U.paramCount === 1 ? 'Nothing' : U.param2
          return isSubtype(U_param1, T.param1) // contravariant
            &&   isSubtype(U_param2, T.param2) // contravariant
            &&   isSubtype(T.result, U.result) // covariant
        }
        // else, T is an N-ary function
        if (U.paramCount === 2) return false
        if (U.paramCount === 1) {
          return T.optionalCount === T.labels.length
            && isSubtype(U.param,  T.firstParam) // contravariant
            && isSubtype(T.result, U.result)     // covariant
        }
        const minUparams = U.labels.length - U.optionalCount
        const minTparams = T.labels.length - T.optionalCount
        if (U.labels.length > T.labels.length
          || minUparams < minTparams) return false
        if (!isSubtype(T.result,     U.result))     return false // covariant
        if (!isSubtype(U.firstParam, T.firstParam)) return false // contravariant
        return U.labels.every((U_label, i) => T.labels[i] === U_label
          && isSubtype(U.params[U_label], T.params[U_label]))    // contravariant
    }
  }

  // union of things that can't be unioned (e.g. string and number) has to
  // be `false` and not `Nothing` because `Nothing` is a subtype of every
  // type, and union(A, B) must always be a supertype of both A and B
  export function union<C extends CompositeType>(T: C, U: C): C | false
  export function union<P extends PrimitiveType>(T: P, U: P): P | false
  export function union<
           t extends NontrivialType, u extends Exclude<NontrivialType, t>
         >(T: t, U: u): false
  export function union(T: NontrivialType, U: NontrivialType): NontrivialType | false
  export function union(T: 'Anything', U: Type): 'Anything'
  export function union(T: Type, U: 'Anything'): 'Anything'
  export function union<t extends Type>(T: 'Nothing', U: t): t
  export function union<t extends Type>(T: t, U: 'Nothing'): t
  export function union(T: Type, U: Type): Type | false
  export function union(T: Type, U: Type): Type | false {
    if (T === 'Anything' || U === 'Anything') return 'Anything'
    if (T === 'Nothing'  || U === 'Nothing')  return T === 'Nothing' ? U : T

    if (T === 'None') return U === 'None' ? U : { ...U, nullable: true }
    if (U === 'None') return { ...T, nullable: true }

    const error = T.error && U.error ? union(T.error, U.error)
      : T.error || U.error
    if (error === false) return false

    const nullable = T.nullable || U.nullable

    if (T.species === 'Error' || U.species === 'Error') {
      return { ...(T.species === 'Error' ? U : T), error: error!, nullable }
    }

    if (U.species !== T.species) return false
    switch (T.species) {
      case 'boolean':
      case 'number':
      case 'string':
        return { species: T.species, error, nullable }
      case 'Array':
        if (U.species !== 'Array') return false
        const ItemType = union(T.ItemType, U.ItemType)
        return ItemType && { species: 'Array', ItemType, error, nullable }
      case 'Product':
        // TODO: implement SumOfProducts
        // for now, just union overlapping keys
        if (U.species !== 'Product') return false
        let nonempty = false
        const fields: {[name: string]: NontrivialType} = {}
        for (const fieldName of Object.keys(T.fields)) {
          if (!(fieldName in U.fields)) continue
          const fieldType = union(T.fields[fieldName], U.fields[fieldName])
          if (!fieldType) continue
          fields[fieldName] = fieldType
          nonempty = true
        }
        return nonempty && { species: 'Product', fields, error, nullable }
      case 'Sum':
        if (U.species !== 'Sum') return false
        const variants: {[tag: string]: NontrivialType | null} = {}
        for (const tag of Object.keys(T.variants)) {
          const T_variant = T.variants[tag]
          if (!(tag in U.variants)) {
            variants[tag] = T_variant
            continue
          }
          const U_variant = U.variants[tag]

          // can't union if one is null but not the other; !== is boolean xor
          if (!T_variant !== !U_variant) return false

          if (T_variant && U_variant) {
            const variant = union(T_variant, U_variant)
            if (!variant) return false
            variants[tag] = variant
          }
          else variants[tag] = null
        }
        for (const tag of Object.keys(U.variants)) {
          if (!(tag in variants)) variants[tag] = U.variants[tag]
        }
        return { species: 'Sum', variants, error, nullable }
      case 'Function':
        // unioning means using one signature to call either of two functions
        // E.g. Let f = (cond ? foo : bar); f(a, thing: b, another: c)
        if (U.species !== 'Function') return false
        const result = union(T.result, U.result) // covariant
        if (!result) return false
        if (T.paramCount === 1) {
          if (U.paramCount === 2  && !U.param2Optional) return false
          if (U.paramCount === 99
            && U.optionalCount < U.labels.length
          ) return false

          const U_param = U.paramCount === 1 ? U.param
            : U.paramCount === 2 ? U.param1
            : U.firstParam
          const param = intersect(T.param, U_param) // contravariant
          if (param === 'Nothing') return false
          return { species: 'Function', paramCount: 1, param, result,
            error, nullable }
        }
        if (T.paramCount === 2) {
          if (U.paramCount === 1) return union(U, T)
          if (U.paramCount === 99) return false

          const param1 = intersect(T.param1, U.param1) // contravariant
          if (param1 === 'Nothing') return false
          const param2 = intersect(T.param2, U.param2) // contravariant
          if (param2 === 'Nothing') return false
          const param2Optional = T.param2Optional && U.param2Optional
          return { species: 'Function', paramCount: 2, param1, param2,
            param2Optional, result, error, nullable }
        }
        // else, T is an N-ary function
        if (U.paramCount === 1) return union(U, T)
        if (U.paramCount === 2) return false

        const firstParam =
          intersect(T.firstParam, U.firstParam)  // contravariant
        if (firstParam === 'Nothing') return false

        const maxLabels = Math.min(T.labels.length, U.labels.length)
        const minRequired = Math.max(
          T.labels.length - T.optionalCount,
          U.labels.length - U.optionalCount)
        if (minRequired > maxLabels) return false

        const labels: string[] = []
        const params: {[label: string]: NontrivialType} = {}
        for (let i = 0; i < maxLabels; i += 1) {
          const label = T.labels[i]
          if (U.labels[i] !== label) break
          const param = intersect(T.params[label], U.params[label]) // contravariant
          if (param === 'Nothing') break
          labels.push(label)
          params[label] = param
        }
        // if either function's required params exceed the params that
        // have been found to match, can't union
        if (T.labels.length - T.optionalCount > labels.length
          || U.labels.length - U.optionalCount > labels.length) return false
        return {
          species: 'Function',
          paramCount: 99,
          firstParam,
          labels,
          params,
          optionalCount: Math.max(labels.length - minRequired, 0),
          result,
          error,
          nullable,
        }
    }
  }

  export function intersect<C extends CompositeType>(T: C, U: C): C | 'Nothing'
  export function intersect<P extends PrimitiveType>(T: P, U: P): P | 'Nothing'
  export function intersect<
    t extends NontrivialType, u extends Exclude<NontrivialType, t>
  >(T: t, U: u): 'Nothing'
  export function intersect(T: NontrivialType, U: NontrivialType): NontrivialType | 'Nothing'
  export function intersect(T: 'Nothing', U: Type): 'Nothing'
  export function intersect(T: Type, U: 'Nothing'): 'Nothing'
  export function intersect<t extends Type>(T: 'Anything', U: t): t
  export function intersect<t extends Type>(T: t, U: 'Anything'): t
  export function intersect(T: Type, U: Type): Type
  export function intersect(T: Type, U: Type): Type {
    if (T === 'Nothing'  || U === 'Nothing')  return 'Nothing'
    if (T === 'Anything' || U === 'Anything') return T === 'Anything' ? U : T

    if (T === 'None') return U === 'None' || U.nullable ? T : 'Nothing'
    if (U === 'None') return T.nullable ? U : 'Nothing'

    let minimum: Err | 'None' | 'Nothing' = 'Nothing'
    const nullable = T.nullable && U.nullable
    if (nullable) minimum = 'None'

    const errorI = T.error && U.error && intersect(T.error, U.error)
    const error = errorI === 'Nothing' ? undefined : errorI
    if (error) minimum = { species: 'Error', error, nullable }

    if (T.species === 'Error' || U.species === 'Error') return minimum
    if (U.species !== T.species) return minimum

    switch (T.species) {
      case 'boolean':
      case 'number':
      case 'string':
        return { species: T.species, error, nullable }
      case 'Array':
        if (U.species !== 'Array') return minimum
        const ItemType = intersect(T.ItemType, U.ItemType)
        return ItemType === 'Nothing' ? minimum
          : { species: 'Array', ItemType, error, nullable }
      case 'Product': {
        if (U.species !== 'Product') return minimum
        let empty: typeof minimum | false = minimum
        const fields: {[name: string]: NontrivialType} = {}
        for (const fieldName of Object.keys(T.fields)) {
          const T_field = T.fields[fieldName]
          if (!(fieldName in U.fields)) {
            fields[fieldName] = T_field
            empty = false
            continue
          }
          const fieldType = intersect(T_field, U.fields[fieldName])
          if (fieldType === 'Nothing') return minimum
          fields[fieldName] = fieldType
          empty = false
        }
        for (const fieldName of Object.keys(U.fields)) {
          if (!(fieldName in fields)) fields[fieldName] = U.fields[fieldName]
        }
        return empty || { species: 'Product', fields, error, nullable }
      }
      case 'Sum':
        if (U.species !== 'Sum') return minimum
        let empty: typeof minimum | false = minimum
        const variants: {[tag: string]: NontrivialType | null} = {}
        for (const tag of Object.keys(T.variants)) {
          if (!(tag in U.variants)) continue
          const T_variant = T.variants[tag]
          const U_variant = U.variants[tag]

          // can't intersect if one is null but not the other; !== is boolean xor
          if (!T_variant !== !U_variant) continue

          if (T_variant && U_variant) {
            const variant = intersect(T_variant, U_variant)
            if (variant === 'Nothing') continue
            variants[tag] = variant
          }
          else variants[tag] = null
          empty = false
        }
        return empty || { species: 'Sum', variants, error, nullable }
      case 'Function':
        // intersection means calling one function using either of two signatures
        // E.g. cond ? foo(a) : foo(b, c)
        if (U.species !== 'Function') return minimum
        const result = intersect(T.result, U.result) // covariant
        if (result === 'Nothing') return minimum
        if (T.paramCount === 1) {
          const U_firstParam = U.paramCount === 1 ? U.param
            : U.paramCount === 2 ? U.param1
            : U.firstParam
          const firstParam = union(T.param, U_firstParam) // contravariant
          if (!firstParam) return minimum

          if (U.paramCount === 1) {
            return { species: 'Function', paramCount: 1,
              param: firstParam, result, error, nullable }
          }
          if (U.paramCount === 2) {
            return { species: 'Function', paramCount: 2, param1: firstParam,
              param2: U.param2, param2Optional: true, result, error, nullable }
          }
          return {
            species: 'Function',
            paramCount: 99,
            firstParam,
            labels: U.labels,
            params: U.params,
            optionalCount: U.labels.length,
            result,
            error,
            nullable,
          }
        }
        if (T.paramCount === 2) {
          if (U.paramCount === 1) return intersect(U, T)
          if (U.paramCount === 99) return minimum

          const param1 = union(T.param1, U.param1) // contravariant
          if (!param1) return minimum
          const param2 = union(T.param2, U.param2) // contravariant
          if (!param2) return minimum
          const param2Optional = T.param2Optional || U.param2Optional
          return { species: 'Function', paramCount: 2, param1, param2,
            param2Optional, result, error, nullable }
        }
        // else, T is an N-ary function
        if (U.paramCount === 1) return intersect(U, T)
        if (U.paramCount === 2) return minimum

        const firstParam = union(T.firstParam, U.firstParam) // contravariant
        if (!firstParam) return minimum

        const minLabels = Math.min(T.labels.length, U.labels.length)
        const minRequred = Math.min(
          T.labels.length - T.optionalCount,
          U.labels.length - U.optionalCount)

        const params: {[label: string]: NontrivialType} = {}
        for (let i = 0; i < minLabels; i += 1) {
          const label = T.labels[i]
          if (U.labels[i] !== label) return minimum
          const param = union(T.params[label], U.params[label])
          if (!param) return minimum
          params[label] = param
        }
        const more = T.labels.length > minLabels ? T : U
        for (const label of more.labels.slice(minLabels)) {
          params[label] = more.params[label]
        }
        return {
          species: 'Function',
          paramCount: 99,
          firstParam,
          labels: more.labels,
          params,
          optionalCount: more.labels.length - minRequred,
          result,
          error,
          nullable,
        }
    }
  }

  export function stringify(T: Type): string {
    if (!T) return `!<${T}>`
    if (T === 'None') return '#none'
    if (typeof T === 'string') return T

    const parenthesize = (t: NontrivialType) => {
      const s = stringify(t)
      return s.indexOf(' ') > -1 ? `(${s})` : s
    }

    if (T.species === 'Error') {
      return (T.nullable ? '#none | ' : '') + '#error '+parenthesize(T.error)
    }
    const errorable = (T.nullable ? ' | #none' : '')
      + (T.error ? ` | #error (${stringify(T.error)})` : '')

    switch (T.species) {
      case 'boolean':
      case 'number':
      case 'string':
        return T.species + errorable
      case 'Array':
        return parenthesize(T.ItemType) + '[]' + errorable
      case 'Product':
        return '{ '
          + Object.entries(T.fields)
            .map(([field, type]) => field + ': ' + stringify(type))
            .sort().join(', ')
          + ' }' + errorable
      case 'Sum':
        return Object.entries(T.variants)
          .map(([tag, variant]) =>
            '#'+tag + (variant ? ' '+ parenthesize(variant) : ''))
          .sort().join(' | ') + errorable
      case 'Function':
        const errable = (s: string) => errorable ? `(${s})`+errorable : s
        if (T.paramCount === 1) {
          return errable(`(${stringify(T.param)}) => ${stringify(T.result)}`)
        }
        if (T.paramCount === 2) {
          return errable('(' + stringify(T.param1) + ', '
            + (T.param2Optional ? parenthesize(T.param2) + '?'
                : stringify(T.param2))
            + ') => ' + stringify(T.result))
        }
        return errable('(' + stringify(T.firstParam) + ', '
          + T.labels.map((label, i) =>
              label + (i >= T.labels.length - T.optionalCount ? '?' : '')
                + ': ' + stringify(T.params[label])
            ).join(', ')
          + ') => ' + stringify(T.result))
    }
    if ((T as {}).toString !== Object.prototype.toString) return `!<${T}>`
    else return `!<${JSON.stringify(T)}>`
  }
}




//
// Codegen: AST -> JS text
//
interface Context {
  indent: string
  scope: { [name: string]: string }
}

export function codegenExpr(ctx: Context, expr: AST.Expression): string {
  if (typeof expr === 'string') {
    // TODO: field access functions, escaping line terminators in string literals
    return expr
  }
  switch (expr.type) {
    case 'Variable':
        return ctx.scope[expr.name]
    case 'ArrayLiteral':
      return `[${expr.exprs.map(item => codegenExpr(ctx, item)).join(', ')}]`
    case 'RecordLiteral':
      if (expr.pairs.length === 0) return '{}'
      if (expr.pairs.length === 1) {
        const [{ key, val }] = expr.pairs
        return `{ ${key}: ${codegenExpr(ctx, val)} }`
      }
      const indent = ctx.indent + '  '
      return `{\n${
        expr.pairs.map(({key, val}) =>
          indent + key + ': ' + codegenExpr({ ...ctx, indent }, val)
        ).join(',\n')
      }\n${ctx.indent}}`
    case 'CallExpr':
      if (expr.contextArg) throw 'method-syntax for calling functions not yet implemented'
      return `${codegenExpr(ctx, expr.func)}(${
        expr.args.map(({arg}) => codegenExpr(ctx, arg)).join(', ')})`
    default:
      throw 'Unexpected or not-yet-implemented expression type: ' + expr.type
  }
}

export function codegenStmt(ctx: Context, stmt: AST.Statement): string {
  switch (stmt.type) {
    case 'DoStmt':
      return `${codegenExpr(ctx, stmt.expr)}();\n`
    default:
      throw 'Unexpected or not-yet-implemented statement type: ' + stmt.type
  }
}

export function compile(source: string) {
  const ast = ProgramParser.tryParse(source)

  // reorder top-level
  const statements: AST.Statement[] = []
  const stateDecls: AST.StateDecl[] = []
  const whenDecls: AST.WhenDecl[] = []
  for (const topLevel of ast) {
    if (topLevel.type === 'StateDecl') stateDecls.push(topLevel)
    else if (topLevel.type === 'WhenDecl') whenDecls.push(topLevel)
    else statements.push(topLevel)
  }

  const topLevelContext: Context = {
    indent: '',
    scope: {},
  }
  const topLevelScope: {[name: string]: string} = {
    console_log: '(...args) => () => console.log(...args)'
  }

  let js = '// runtime:\n'
  for (const name in topLevelScope) {
    js += `const ${name} = ${topLevelScope[name]};\n`
    topLevelContext.scope[name] = name
  }

  js += '\n// State declarations: (TODO)\n'

  js += '\n// initializing statements:\n'
  for (const statement of statements) {
    js += codegenStmt(topLevelContext, statement)
  }

  js += '\n// When declarations: (TODO)\n'

  return js
}




//
// Command-line parsing
//
if (typeof require !== 'undefined' && typeof module !== 'undefined' && require.main === module) {
  const argv = process.argv
  if (argv.includes('-h') || argv.includes('--help')) {
    console.error('Usage: npx mechc some_file.mech     # writes the compiled output to some_file.js\n'
                + '   or: npx mechc < some_file.mechc  # writes the compiled output to stdout\n'
                + '\n'
                + 'For more info, see: https://github.com/laughinghan/mechanical')
    process.exit()
  }
  if (argv.length > 3) {
    console.error('Sorry, mechc only supports 0 or 1 arguments')
    process.exit(1)
  }

  const sourceFile = process.argv[2] || 0 // fd 0 is stdin
  if (sourceFile && sourceFile.slice(-5) !== '.mech') {
    console.error('Sorry, mechc can only compile .mech files')
    process.exit(1)
  }
  const outFile = sourceFile ? sourceFile.slice(0, -5) + '.js' : 1 // fd 1 is stdout

  const sourceText = fs.readFileSync(sourceFile, { encoding: 'utf8' })
  const outText = `// compiled by Mechanical v0.0.1 from ${sourceFile}\n\n` + compile(sourceText)
  fs.writeFileSync(outFile, outText)
}
