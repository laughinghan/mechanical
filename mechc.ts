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

export const parser = createLanguage({
  // Whitespace
  _: () => r(/[ \t]*/),  // optional, non-newline whitespace
  __: () => r(/[ \t]+/), // required, non-newline whitespace
  _n: () => r(/[ \t\n]+/), // newline or whitespace---no other whitespace allowed,
                                 // \a\b\v\f\r are all banned.
                                 // Banned whitespace should be treated kinda like control
                                 // characters and non-printing characters

  //
  // Expression Grammar (based on JS)
  //   https://tc39.es/ecma262/#sec-ecmascript-language-expressions
  //
  Identifier: () => r(/[a-z]\w*/i), // TODO non-English letters etc
  Numeral: () => r(/\d+/), // TODO decimals, exponential notation

  PrimaryExpr: L => alt( // in terms of operator precedence, "primary expressions"
      // are the smallest units, which are either actual leaf nodes (variables,
      // numberals, string literals) or delimited groups (parenthesized exprs,
      // array literals, object literals, function calls, anonymous functions).
      // They are the operands to the tightest-binding operator
    L.Identifier,
    L.Numeral,
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
    )

  // top-level statements in an event handler declaration
})
