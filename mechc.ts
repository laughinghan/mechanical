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

namespace AST {
  export type Expression = PrimaryExpr | FieldAccessExpr | CallExpr | UnaryExpr
    | BinaryExpr | CompareChainExpr | CondExpr | ArrowFunc

  export type PrimaryExpr = string | ArrayLiteral | RecordLiteral
  export interface ArrayLiteral {
    type: 'ArrayLiteral'
    exprs: Expression[]
  }
  export interface RecordLiteral {
    type: 'RecordLiteral'
    pairs: Array<{
      key: string
      val: Expression
    }>
  }

  export interface FieldAccessExpr {
    type: 'FieldAccessExpr'
    record: Expression
    fieldName: string
  }
  export interface CallExpr {
    type: 'CallExpr'
    contextArg: Expression | null
    func: Expression
    args: Array<{
      label: string | null
      arg: Expression
    }>
  }
  export interface UnaryExpr {
    type: 'UnaryExpr'
    op: '-' | '!'
    arg: Expression
  }
  export interface BinaryExpr {
    type: 'BinaryExpr'
    op: '**' | '*' | '/' | '%' | '+' | '-' | '!=' | '==' | '<' | '>' | '<='
      | '>=' | '&&' | '||' // in order of precedence
    left: Expression
    right: Expression
  }
  export interface CompareChainExpr {
    type: 'CompareChainExpr'
    chain: BinaryExpr[]
  }
  export interface CondExpr {
    type: 'CondExpr'
    test: Expression
    ifYes: Expression
    ifNo: Expression
  }
  export interface ArrowFunc {
    type: 'ArrowFunc'
    params: string[]
    body: Expression | Statement[]
  }


  export type Statement = ReturnStmt | EmitStmt | LetStmt | ChangeStmt
    | DoStmt | GetDoStmt | FutureDoStmt | AfterGotStmt

  export interface ReturnStmt {
    type: 'ReturnStmt'
    expr: Expression
  }
  export interface EmitStmt {
    type: 'EmitStmt'
    expr: Expression
  }

  export interface LetStmt {
    type: 'LetStmt'
    varName: string
    expr: Expression
  }
  export interface ChangeStmt {
    type: 'ChangeStmt'
    varName: string
    expr: Expression
  }

  export interface DoStmt {
    type: 'DoStmt'
    expr: Expression
  }
  export interface GetDoStmt {
    type: 'GetDoStmt'
    varName: string
    expr: Expression
  }
  export interface FutureDoStmt {
    type: 'FutureDoStmt'
    varName: string
    expr: Expression
  }
  export interface AfterGotStmt {
    type: 'AfterGotStmt'
    vars: string[]
  }


  export interface StateDecl {
    type: 'StateDecl'
    varName: string
    expr: Expression
  }
  export interface WhenDecl {
    type: 'WhenDecl'
    event: Expression
    varName: string | null
    body: Statement[]
  }

  export type TopLevel = StateDecl | WhenDecl | DoStmt | GetDoStmt
}

// who says you can't do left-recursion in a top-down parser? Come at me!
function leftRecur<Op, BinExpr, ArgExpr extends BinExpr>(
  argParser: Parser<ArgExpr>,
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
    .map<AST.ArrayLiteral>(exprs => ({ type: 'ArrayLiteral', exprs }))
  ).skip(s(']'))
    .desc('array literal (e.g. [ ... ])')
  const RecordLiteral = s('{').then(
    seqMap(Identifier, alt(s(':').trim(_).then(Expression), succeed(null)),
      (key, val) => ({ key, val: val ?? key }))
    .thru(sepByOptTrailing(s(',').trim(_))).trim(_)
    .map<AST.RecordLiteral>(pairs => ({ type: 'RecordLiteral', pairs }))
  ).skip(s('}'))
    .desc('record literal (e.g. { ... })')

  const PrimaryExpr = alt( // in terms of operator precedence, "primary expressions"
      // are the smallest units, which are either actual leaf nodes (variables,
      // numberals, string literals) or delimited groups (parenthesized exprs,
      // array literals, record literals, function calls, anonymous functions).
      // They are the operands to the tightest-binding operator
    ParenGroup,
    Identifier,
    Numeral,
    FieldFunc,
    StringLiteral,
    ArrayLiteral,
    RecordLiteral,
  )
  const FieldAccessExpr = alt<AST.Expression>( // (tightest-binding operator)
    seqMap(PrimaryExpr.skip(s('.').trim(_)), Identifier,
      (record, fieldName) => ({ type: 'FieldAccessExpr', record, fieldName })),
    PrimaryExpr,
  )
  const CallExpr = alt<AST.Expression>( // as an exception to operator precedence, binds tighter
    seqMap( // leftwards than FieldAccessExpr, because of how '.' is overloaded
      PrimaryExpr.skip(_),
      s('.').then(Identifier.trim(_)).or(succeed(null)),
      s('(').then(
        seqMap(Identifier.skip(s(':').trim(_)).or(succeed(null)), Expression,
          (label, arg) => ({ label, arg })).sepBy(s(',').trim(_)).trim(_)
      ).skip(s(')')),
      (expr, infixIdent, args) => infixIdent === null
        ? { type: 'CallExpr', contextArg: null, func: expr,       args }
        : { type: 'CallExpr', contextArg: expr, func: infixIdent, args }
    ),
    FieldAccessExpr,
  )
  const UnaryExpr = alt<AST.Expression>(
    seqMap(r(/[-!]/).skip(_) as Parser<'-' | '!'>, CallExpr,
      (op, arg) => ({ type: 'UnaryExpr', op, arg })),
    CallExpr,
  )
  const ExponentExpr = alt<AST.Expression>(
    seqMap(CallExpr.skip(s('**').trim(_)), UnaryExpr,
      (left, right) => ({ type: 'BinaryExpr', op: '**', left, right })),
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
  const MultExpr: Parser<AST.Expression> = leftRecur(
    ExponentExpr, r(/[*/%]/).trim(_) as Parser<'*' | '/' | '%'>,
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right })
  )
  const AddExpr: Parser<AST.Expression> = leftRecur(
    MultExpr, r(/[+-]/).trim(_) as Parser<'+' | '-'>,
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right })
  )
  const InequalityExpr: Parser<AST.BinaryExpr> = seqMap( // mutually
      // exclusive precedence with other comparisons and cannot be chained,
      // because (1 != 2 != 1) == #yes could be surprising, but anything else
      // would require quadratic comparisons
    AddExpr.skip(s('!=').trim(_)), AddExpr,
    (left, right) => ({ type: 'BinaryExpr', op: '!=', left, right })
  )
  const CompareChainExpr: Parser<AST.Expression> = seqMap(
    AddExpr,
    seqMap(
      seq(s('==' as string).trim(_), AddExpr).many(), // we need to special-case the
        // leading =='s because alt() requires failure to fall-through to the
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
          }
        : ({
            type: 'CompareChainExpr',
            chain: rest.map(([op, arg], i) => ({
              type: 'BinaryExpr',
              op: op as AST.BinaryExpr['op'],
              left: i ? rest[i-1][1] : first,
              right: arg,
            })),
          })
  )
  const CompareExpr = alt(InequalityExpr, CompareChainExpr)
  const AndExpr: Parser<AST.Expression> = leftRecur( // conventionally higher
    CompareExpr, s('&&').trim(_),                    // precedence than OrExpr
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right })
  )
  const OrExpr: Parser<AST.Expression> = leftRecur( // conventionally lower
    AndExpr, s('||').trim(_),                       // precedence than AndExpr
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right })
  )
  const CondExpr = alt<AST.Expression>(
    seqMap(
      OrExpr.skip(s('?').trim(_)), Expression.skip(s(':').trim(_)), Expression,
      (test, ifYes, ifNo) => ({ type: 'CondExpr', test, ifYes, ifNo })
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
    (params, body) => ({ type: 'ArrowFunc', params, body })
  ))


  //
  // Statements
  // allowed in the body of an event handler declaration, or in a cmd {} block
  //
  const ReturnStmt = s('Return').then(__).then(Expression)
    .map<AST.ReturnStmt>(expr => ({ type: 'ReturnStmt', expr }))
  const EmitStmt = s('Emit').then(__).then(Expression)
    .map<AST.EmitStmt>(expr => ({ type: 'EmitStmt', expr }))

  const LetStmt: Parser<AST.LetStmt> = seqMap(
    s('Let').then(__).then(Identifier), s('=').trim(_).then(Expression),
    (varName, expr) => ({ type: 'LetStmt', varName, expr }),
  )
  const ChangeStmt: Parser<AST.ChangeStmt> = seqMap(
    s('Change').then(__).then(Identifier), s('to').trim(_).then(Expression),
    (varName, expr) => ({ type: 'ChangeStmt', varName, expr })
  )

  const DoStmt = s('Do').then(__).then(Expression)
    .map<AST.DoStmt>(expr => ({ type: 'DoStmt', expr }))
  const GetDoStmt: Parser<AST.GetDoStmt> = seqMap(
    s('Get').then(__).then(Identifier),
    s('=').trim(_).then(s('Do')).then(_).then(Expression),
    (varName, expr) => ({ type: 'GetDoStmt', varName, expr })
  )
  const FutureDoStmt: Parser<AST.FutureDoStmt> = seqMap(
    s('Future').then(__).then(Identifier),
    s('=').trim(_).then(s('Do')).then(_).then(Expression),
    (varName, expr) => ({ type: 'FutureDoStmt', varName, expr })
  )
  const AfterGotStmt: Parser<AST.AfterGotStmt> = r(/~* *After +got +/).then(
    Identifier.sepBy1(s(',').trim(_))
    .desc('at least one variable required in "After got ..." statement')
  ).skip(r(/ *~*/)).map(vars => ({ type: 'AfterGotStmt', vars }))

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
const StateDecl: Parser<AST.StateDecl> = seqMap(
  s('State').then(__).then(Identifier), s('=').trim(_).then(parser.Expression),
  (varName, expr) => ({ type: 'StateDecl', varName, expr })
)
const WhenDecl: Parser<AST.WhenDecl> = seqMap(
  s('When').then(__).then(parser.Expression),
  alt(s('with').trim(__).then(Identifier), succeed(null))
  .skip(_).skip(s(':')).skip(_EOL),
  parser.StatementIndentBlock,
  (event, varName, body) => ({ type: 'WhenDecl', event, varName, body })
)
export const TopLevel =
  alt<AST.TopLevel>(StateDecl, WhenDecl, parser.DoStmt, parser.GetDoStmt)


export const ProgramParser = s('Mechanical v0.0.1\n').then(
  alt(TopLevel, _nonNL.result(null))
  .sepBy(_EOL.desc('Top-level declarations cannot be indented'))
).map((decls => decls.filter(Boolean)) as <T>(decl: T[]) => Exclude<T, null>[])




//
// Compilation: AST -> JS text
//
function compileExpr(scope: {[k: string]: string}, expr: AST.Expression): string {
  if (typeof expr === 'string') {
    // TODO: field access functions, escaping line terminators in string literals
    return expr
  }
  switch (expr.type) {
    case 'CallExpr':
      if (expr.contextArg) throw 'method-syntax for calling functions not yet implemented'
      return `${compileExpr(scope, expr.func)}(${
        expr.args.map(({arg}) => compileExpr(scope, arg)).join(', ')})`
    default:
      throw 'Unexpected or not-yet-implemented expression type: ' + expr.type
  }
}

function compileStmt(scope: {[k: string]: string}, stmt: AST.Statement) {
  switch (stmt.type) {
    case 'DoStmt':
      return `${compileExpr(scope, stmt.expr)}();\n`
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

  let js = ''
  const topLevelScope: {[k: string]: string} = {
    console_log: '(...args) => () => console.log(...args)'
  }

  // runtime
  js += '// runtime:\n'
  for (const name in topLevelScope) {
    js += `const ${name} = ${topLevelScope[name]};\n`
  }
  js += '\n'

  // TODO: compile State declarations

  // compile statements
  for (const statement of statements) js += compileStmt(topLevelScope, statement)

  // TODO: compile When declarations
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
