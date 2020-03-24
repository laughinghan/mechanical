import {
  Parser,
  succeed,
  createLanguage,
  alt,
  seq,
  seqMap,
  seqObj,
  string as s,
  regex as r
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

export const parser = createLanguage({
  // Whitespace
  _: () => r(/[ \n]*/),  // optional whitespace
  __: () => r(/[ \n]+/), // required whitespace
  // Note that Tabs are banned, as are exotic whitespace \a\b\v\f\r. Banned
  // whitespace should be treated like control characters and non-printing
  // characters
  EOL: () => r(/[ \n]*(?:\/\/[^\n]*)?\n/).desc('end-of-line'), // end-of-line,
    // including optional whitespace and line comment


  //
  // Expression Grammar (based on JS)
  //   https://tc39.es/ecma262/#sec-ecmascript-language-expressions
  //
  Identifier: () => r(/[a-z](?:[a-z0-9]|_[a-z0-9])*/i) // TODO non-English letters etc
    .desc('identifier (e.g. example_identifier)'),
  Numeral: () => r(/\d+/).desc('numeral (e.g. 123)'), // TODO decimals, exponential notation
  StringLiteral: () => r(/"(?:\\"|[^"])*"|'(?:\\'|[^'])*'/)
    .desc(`string literal (e.g. "..." or '...')`),
  ArrayLiteral: ({ _, Expression }) => s('[').then(
    Expression.thru(sepByOptTrailing(s(',').trim(_))).trim(_)
    .map(exprs => ({ type: 'ArrayLiteral', exprs }))
  ).skip(s(']'))
    .desc('array literal (e.g. [ ... ])'),
  RecordLiteral: ({ _, Identifier, Expression }) => s('{').then(
    seqMap(Identifier, alt(s(':').trim(_).then(Expression), succeed(null)),
      (key, val) => (val === null ? { key, val: key } : { key, val }))
    .thru(sepByOptTrailing(s(',').trim(_))).trim(_)
    .map(pairs => ({ type: 'RecordLiteral', pairs }))
  ).skip(s('}'))
    .desc('record literal (e.g. { ... })'),
  ArrowFunc: ({ _, Identifier, StatementBlock, Expression }) => seqMap(
    alt(
      Identifier.map(Array),
      s('(').then(Identifier.sepBy1(s(',').trim(_)).trim(_)).skip(s(')')),
    )
    .skip(s('=>').trim(_)),
    alt(
      s('{').then(StatementBlock.trim(_)).skip(s('}')),
      Expression,
    ),
    (params, body) => ({ type: 'ArrowFunc', params, body })
  ),

  PrimaryExpr: L => alt( // in terms of operator precedence, "primary expressions"
      // are the smallest units, which are either actual leaf nodes (variables,
      // numberals, string literals) or delimited groups (parenthesized exprs,
      // array literals, record literals, function calls, anonymous functions).
      // They are the operands to the tightest-binding operator
    L.ArrowFunc,
    L.Identifier,
    L.Numeral,
    L.StringLiteral,
    L.ArrayLiteral,
    L.RecordLiteral,
  ),
  UnaryExpr: ({_, PrimaryExpr }) => alt( // (tightest-binding operator)
    PrimaryExpr,
    seqMap(r(/[-!]/), _, PrimaryExpr,
      (op, _, arg) => ({ type: 'UnaryExpr', op, arg })),
  ),
  ExponentExpr: ({ PrimaryExpr, _, UnaryExpr }) => alt(
    seqMap(PrimaryExpr, s('**').trim(_), UnaryExpr,
      (left, _, right) => ({ type: 'BinaryExpr', op: '**', left, right })),
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
  ),
  MultExpr: ({ ExponentExpr, _ }) => leftRecur(
    ExponentExpr, r(/[*/%]/).trim(_),
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right })
  ),
  AddExpr: ({ MultExpr, _ }) => leftRecur(
    MultExpr, r(/[+-]/).trim(_),
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right })
  ),
  InequalityExpr: ({ AddExpr, _ }) => seqMap( // mutually exclusive precedence with
      // other comparisons and may not be chained, because (1 != 2 != 1) == #yes
      // would be weird, but anything else would require quadratic comparisons
    AddExpr, s('!=').trim(_), AddExpr,
    (left, _, right) => ({ type: 'InequalityExpr', left, right })
  ),
  CompareChainExpr: ({ AddExpr, _ }) => seqMap(
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
    (first, rest) => rest.length
      ? ({
          type: 'CompareChainExpr',
          chain: rest.map(([op, arg], i) => ({
            type: 'BinaryExpr',
            op,
            left: i ? rest[i-1][1] : first,
            right: arg,
          })),
        })
      : first
  ),
  CompareExpr: ({ InequalityExpr, CompareChainExpr }) =>
    alt(InequalityExpr, CompareChainExpr),
  AndExpr: ({ CompareExpr, _ }) => leftRecur( // conventionally higher precedence
                                              // than OrExpr
    CompareExpr, s('&&').trim(_),
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right })
  ),
  OrExpr: ({ AndExpr, _ }) => leftRecur( // conventionally lower precedence
                                         // than AndExpr
    AndExpr, s('||').trim(_),
    (op, left, right) => ({ type: 'BinaryExpr', op, left, right })
  ),
  CondExpr: ({ CondExpr, OrExpr, _ }) => alt(
    seqMap(OrExpr, s('?').trim(_), CondExpr, s(':').trim(_), CondExpr,
      (test, _, ifYes, __, ifNo) => ({ type: 'CondExpr', test, ifYes, ifNo })),
    OrExpr,
  ),
  Expression: ({ CondExpr }) => CondExpr,


  //
  // Statements
  // allowed in the body of an event handler declaration, or in a cmd {} block
  //
  LetStmt: ({ _, __, Identifier, Expression, EOL }) => s('Let').skip(__).then(
    seqMap(Identifier.skip(seq(s('=').trim(_))), Expression,
      (varName, expr) => ({ type: 'LetStmt', varName, expr }))
  ).skip(EOL),
  StatementBlock: L => L.LetStmt,


  //
  // Top-Level Declarations
  //
  StateDecl: ({ _, __, Identifier }) =>
    seqObj<{var_name: string}, 'var_name'>(
      s('State'),
      __,
      ['var_name', Identifier],
      _,
      s('='),
      _,
    ),
})
