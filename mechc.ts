import P from "parsimmon"

const parser = P.createLanguage({
  // whitespace
  _: () => P.regex(/[ \t]*/), // optional, non-newline whitespace
  __: () => P.regex(/[ \t]+/), // required, non-newline whitespace
  _n: () => P.regex(/[ \t\n]+/), // newline or whitespace---no other whitespace allowed,
                               // \a\b\v\f\r are all banned.
                               // Banned whitespace should be treated kinda like control
                               // characters and non-printing characters

  // expression grammar (based on JS)
  Identifier: () => P.regex(/[a-z]\w*/i), // TODO non-English letters etc
  Numeral: () => P.regex(/[+-]?\d+/), // TODO decimals, exponential notation

  // top-level declarations
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
