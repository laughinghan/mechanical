import {
  Parser,
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

  PrimaryExpr: L => alt( // in terms of operator precedence,
      // "primary expressions" are the smallest units, which are either actual
      // leaf nodes (variables, numberals, string literals) or delimited groups
      // (parenthesized exprs, array literals, object literals, function calls,
      // anonymous functions). They are the operands to the tightest-binding
      // operator
    L.Identifier,
    L.Numeral,
  ),

  UnaryExpr: ({_, PrimaryExpr }) => alt( // unary operators (tightest-binding operators)
    PrimaryExpr,
    seqMap(r(/[-!]/), _, PrimaryExpr,
      (op, _, arg) => ({ type: 'UnaryExpr', op, arg })),
  ),

  ExponentExpr: ({ PrimaryExpr, _, UnaryExpr }) => alt(
    seqMap(PrimaryExpr, _, s('**'), _, UnaryExpr,
      (left, _, __, ___, right) => ({ type: 'BinaryExpr', op: '**', left, right })),
        // Note 1: as a special exception to typical operator precedence,
        // exponents binds equally tightly leftwards as UnaryExpr, to avoid the
        // ambiguity of whether -2**2 is (-2)**2 = 4 or -(2**2) = -4
        //   https://tc39.es/ecma262/#sec-exp-operator
        // Chrome throws:
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
