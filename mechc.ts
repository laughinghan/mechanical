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

// who says you can't do left-recursion in a top-down parser? Come at me!
function leftRecur<Expr, Op>(
  argParser: Parser<Expr>,
  opParser: Parser<Op>,
  map: (op: Op, left: Expr, right: Expr) => Expr
): Parser<Expr> {
  return seqMap(argParser, seq(opParser, argParser).many(), (first, rest) =>
    rest.reduce((left, [op, right]) => map(op, left, right), first))
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


export function parserAtIndent(indent: string) {
  //
  // Expression Grammar (based on JS)
  //   https://tc39.es/ecma262/#sec-ecmascript-language-expressions
  //
  const Expression: Parser<any> = lazy(() => alt(ArrowFunc, CondExpr))

  const ParenGroup = s('(').then(Expression.trim(_)).skip(s(')'))
  const Numeral = r(/\d+/).desc('numeral (e.g. 123)') // TODO decimals, exponential notation
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
    .map(exprs => ({ type: 'ArrayLiteral', exprs }))
  ).skip(s(']'))
    .desc('array literal (e.g. [ ... ])')
  const RecordLiteral = s('{').then(
    seqMap(Identifier, alt(s(':').trim(_).then(Expression), succeed(null)),
      (key, val) => ({ key, val: val ?? key }))
    .thru(sepByOptTrailing(s(',').trim(_))).trim(_)
    .map(pairs => ({ type: 'RecordLiteral', pairs }))
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
  const FieldAccessExpr = alt( // (tightest-binding operator)
    seqMap(PrimaryExpr.skip(s('.').trim(_)), Identifier,
      (record, fieldName) => ({ type: 'FieldAccessExpr', record, fieldName })),
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
      (expr, infixIdent, args) => infixIdent === null
        ? { type: 'CallExpr', contextArg: null, func: expr,       args }
        : { type: 'CallExpr', contextArg: expr, func: infixIdent, args }
    ),
    FieldAccessExpr,
  )
  const UnaryExpr = alt(
    seqMap(r(/[-!]/).skip(_), CallExpr,
      (op, arg) => ({ type: 'UnaryExpr', op, arg })),
    CallExpr,
  )
  const ExponentExpr = alt(
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
  const MultExpr = leftRecur(
    ExponentExpr, r(/[*/%]/).trim(_),
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right })
  )
  const AddExpr = leftRecur(
    MultExpr, r(/[+-]/).trim(_),
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right })
  )
  const InequalityExpr = seqMap( // mutually exclusive precedence with
      // other comparisons and may not be chained, because (1 != 2 != 1) == #yes
      // would be weird, but anything else would require quadratic comparisons
    AddExpr.skip(s('!=').trim(_)), AddExpr,
    (left, right) => ({ type: 'InequalityExpr', left, right })
  )
  const CompareChainExpr = seqMap(
    AddExpr,
    seqMap(
      seq(s('==').trim(_), AddExpr).many(), // we need to special-case the
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
            op: rest[0][0],
            left: first,
            right: rest[0][1],
          }
        : ({
            type: 'CompareChainExpr',
            chain: rest.map(([op, arg], i) => ({
              type: 'BinaryExpr',
              op,
              left: i ? rest[i-1][1] : first,
              right: arg,
            })),
          })
  )
  const CompareExpr = alt(InequalityExpr, CompareChainExpr)
  const AndExpr = leftRecur( // conventionally higher precedence than OrExpr
    CompareExpr, s('&&').trim(_),
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right })
  )
  const OrExpr = leftRecur( // conventionally lower precedence than AndExpr
    AndExpr, s('||').trim(_),
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right })
  )
  const CondExpr = alt(
    seqMap(
      OrExpr.skip(s('?').trim(_)), Expression.skip(s(':').trim(_)), Expression,
      (test, ifYes, ifNo) => ({ type: 'CondExpr', test, ifYes, ifNo })
    ),
    OrExpr,
  )
  const ArrowFunc = lazy(() => seqMap(
    alt(
      Identifier.map(param => [param]),
      s('(').then(Identifier.sepBy1(s(',').trim(_)).trim(_)).skip(s(')')),
    )
    .skip(s('=>').trim(_)),
    alt(
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
    .map(expr => ({ type: 'ReturnStmt', expr }))
  const EmitStmt = s('Emit').then(__).then(Expression)
    .map(expr => ({ type: 'EmitStmt', expr }))

  const LetStmt = seqMap(
    s('Let').then(__).then(Identifier), s('=').trim(_).then(Expression),
    (varName, expr) => ({ type: 'LetStmt', varName, expr }),
  )
  const ChangeStmt = seqMap(
    s('Change').then(__).then(Identifier), s('to').trim(_).then(Expression),
    (varName, expr) => ({ type: 'ChangeStmt', varName, expr })
  )

  const DoStmt = s('Do').then(__).then(Expression)
    .map(expr => ({ type: 'DoStmt', expr }))
  const GetDoStmt = seqMap(
    s('Get').then(__).then(Identifier),
    s('=').trim(_).then(s('Do')).then(_).then(Expression),
    (varName, expr) => ({ type: 'GetDoStmt', varName, expr })
  )
  const FutureDoStmt = seqMap(
    s('Future').then(__).then(Identifier),
    s('=').trim(_).then(s('Do')).then(_).then(Expression),
    (varName, expr) => ({ type: 'FutureDoStmt', varName, expr })
  )
  const AfterGotStmt = r(/~* *After +got +/).then(
    Identifier.sepBy1(s(',').trim(_))
    .desc('at least one variable required in "After got ..." statement')
  ).skip(r(/ *~*/)).map(vars => ({ type: 'AfterGotStmt', vars }))

  const Statement = alt(ReturnStmt, EmitStmt, LetStmt, ChangeStmt,
    DoStmt, GetDoStmt, FutureDoStmt, AfterGotStmt)

  const StatementIndentBlock = _EOL.many().then(_nonNL).chain(newIndent => {
    if (newIndent.length > indent.length) {
      const { Statement }: { Statement: Parser<any> } = parserAtIndent(newIndent)
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

  return { Expression, Statement, StatementIndentBlock }
}

export const parser = parserAtIndent('')


//
// Top-Level Declarations
// all exported
//
const StateDecl = seqMap(
  s('State').then(__).then(Identifier), s('=').trim(_).then(parser.Expression),
  (varName, expr) => ({ type: 'StateDecl', varName, expr })
)
const WhenDecl = seqMap(
  s('When').then(__).then(parser.Expression),
  alt(s('with').trim(__).then(Identifier), succeed(null))
  .skip(_).skip(s(':')).skip(_EOL),
  parser.StatementIndentBlock,
  (event, varName, body) => ({ type: 'WhenDecl', event, varName, body })
)
export const Declaration = alt(StateDecl, WhenDecl)


export const ProgramParser = s('Mechanical v0.0.1\n').then(
  alt(Declaration, _nonNL.result(null))
  .sepBy(_EOL.desc('Top-level declarations cannot be indented'))
).map(decls => decls.filter(Boolean))
