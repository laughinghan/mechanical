import P from 'parsimmon'

export const parser = P.createLanguage({
  // Whitespace
  _: () => P.regex(/[ \t]*/),  // optional, non-newline whitespace
  __: () => P.regex(/[ \t]+/), // required, non-newline whitespace
  _n: () => P.regex(/[ \t\n]+/), // newline or whitespace---no other whitespace allowed,
                                 // \a\b\v\f\r are all banned.
                                 // Banned whitespace should be treated kinda like control
                                 // characters and non-printing characters

  //
  // Expression Grammar (based on JS)
  //   https://tc39.es/ecma262/#sec-ecmascript-language-expressions
  //
  Identifier: () => P.regex(/[a-z]\w*/i), // TODO non-English letters etc
  Numeral: () => P.regex(/[+-]?\d+/), // TODO decimals, exponential notation (negation?)

  PrimaryExpr: L => P.alt( // in terms of operator precedence,
      // "primary expressions" are the smallest units, which are either actual
      // leaf nodes (variables, numberals, string literals) or delimited groups
      // (parenthesized exprs, array literals, object literals, function calls,
      // anonymous functions). They are the operands to the tightest-binding
      // operator
    L.Identifier,
    L.Numeral,
  ),

  UnaryExpr: L => P.alt( // unary operators (tightest-binding operators)
    P.seqMap(P.regex(/[-!]/), L._, L.PrimaryExpr,
      (op, _, arg) => ({ type: 'Unary', op, arg })),
    L.PrimaryExpr,
  ),

  ExponentiationExpr: L => P.alt(
    P.seqMap(L.PrimaryExpr, L._, P.string('**'), L._, L.UnaryExpr,
      (base, _, __, ___, exponent) => ({ type: 'Exponentiation', base, exponent })),
        // note that as a special exception to typical operator precedence,
        // exponentiation binds equally tightly leftwards as UnaryExpr, to avoid
        // the ambiguity of whether -2**2 is (-2)**2 = 4 or -(2**2) = -4
        //   https://tc39.es/ecma262/#sec-exp-operator
    L.PrimaryExpr,
  ),

  //
  // Top-Level Declarations
  //
  StateDecl: ({ _, __, Identifier }) =>
    P.seqObj<{var_name: string}, 'var_name'>(
      P.string('State'),
      __,
      ['var_name', Identifier],
      _,
      P.string('='),
      _,
    )

  // top-level statements in an event handler declaration
})

console.log(parser.StateDecl.tryParse('State x = '))
