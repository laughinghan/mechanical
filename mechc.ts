import {
  Parser,
  makeFailure,
  succeed,
  fail,
  createLanguage,
  alt,
  seq,
  seqMap,
  seqObj,
  lookahead,
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

export function parserAtIndent(indent: string) {
  return createLanguage({
    // Whitespace
    _: () => r(/[ \n]*/),  // optional whitespace
    __: () => r(/[ \n]+/), // required whitespace
    _nonNL: () => r(/ */), // whitespace, no newlines
    _EOL: () => r(/ *(?:\/\/[^\n]*)?\n/).desc('end-of-line'), // end-of-line,
      // including optional whitespace and line comment

    // Note that Tabs are banned, as are exotic whitespace \a\b\v\f\r, except
    // in string literals. Just think of banned whitespace like control
    // characters and non-printing characters.

    // By convention, rules all start and end on non-whitespace, and expect
    // the parent rule to deal with surrounding whitespace, EXCEPT
    // StatementIndentBlock, which expects leading indentation, and consumes
    // a trailing newline.



    //
    // Expression Grammar (based on JS)
    //   https://tc39.es/ecma262/#sec-ecmascript-language-expressions
    //
    Identifier: () => r(/[a-z](?:[a-z0-9]|_[a-z0-9])*/i) // TODO non-English letters etc
      .desc('identifier (e.g. example_identifier)'),
    Numeral: () => r(/\d+/).desc('numeral (e.g. 123)'), // TODO decimals, exponential notation
    StringLiteral: () => r(/"(?:\\"|[^"])*"|'(?:\\'|[^'])*'/)
      .desc(`string literal (e.g. "..." or '...')`)
      .mark().chain(({value: str, start, end}) => {
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
      }),
    ArrayLiteral: ({ _, Expression }) => s('[').then(
      Expression.thru(sepByOptTrailing(s(',').trim(_))).trim(_)
      .map(exprs => ({ type: 'ArrayLiteral', exprs }))
    ).skip(s(']'))
      .desc('array literal (e.g. [ ... ])'),
    RecordLiteral: ({ _, Identifier, Expression }) => s('{').then(
      seqMap(Identifier, alt(s(':').trim(_).then(Expression), succeed(null)),
        (key, val) => ({ key, val: val ?? key }))
      .thru(sepByOptTrailing(s(',').trim(_))).trim(_)
      .map(pairs => ({ type: 'RecordLiteral', pairs }))
    ).skip(s('}'))
      .desc('record literal (e.g. { ... })'),
    ParenGroup: ({ _, Expression }) =>
      s('(').then(Expression.trim(_)).skip(s(')')),

    PrimaryExpr: L => alt( // in terms of operator precedence, "primary expressions"
        // are the smallest units, which are either actual leaf nodes (variables,
        // numberals, string literals) or delimited groups (parenthesized exprs,
        // array literals, record literals, function calls, anonymous functions).
        // They are the operands to the tightest-binding operator
      L.ParenGroup,
      L.Identifier,
      L.Numeral,
      L.StringLiteral,
      L.ArrayLiteral,
      L.RecordLiteral,
    ),
    UnaryExpr: ({_, PrimaryExpr }) => alt( // (tightest-binding operator)
      seqMap(r(/[-!]/).skip(_), PrimaryExpr,
        (op, arg) => ({ type: 'UnaryExpr', op, arg })),
      PrimaryExpr,
    ),
    ExponentExpr: ({ PrimaryExpr, _, UnaryExpr }) => alt(
      seqMap(PrimaryExpr.skip(s('**').trim(_)), UnaryExpr,
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
      AddExpr.skip(s('!=').trim(_)), AddExpr,
      (left, right) => ({ type: 'InequalityExpr', left, right })
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
    CondExpr: ({ OrExpr, Expression, _ }) => alt(
      seqMap(OrExpr.skip(s('?').trim(_)), Expression.skip(s(':').trim(_)), Expression,
        (test, ifYes, ifNo) => ({ type: 'CondExpr', test, ifYes, ifNo })),
      OrExpr,
    ),
    ArrowFunc: ({ _, Identifier, Expression, StatementBraceBlock }) => seqMap(
      alt(
        Identifier.map(Array),
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
    ),
    Expression: ({ ArrowFunc, CondExpr }) => alt(ArrowFunc, CondExpr),


    //
    // Statements
    // allowed in the body of an event handler declaration, or in a cmd {} block
    //
    LetStmt: ({ _, __, Identifier, Expression }) => seqMap(
      s('Let').then(__).then(Identifier), s('=').trim(_).then(Expression),
      (varName, expr) => ({ type: 'LetStmt', varName, expr }),
    ),
    ReturnStmt: ({ _, __, Expression }) =>
      s('Return').then(__).then(Expression)
      .map(expr => ({ type: 'ReturnStmt', expr })),

    Statement: L => alt(
      L.LetStmt,
      L.ReturnStmt,
    ),

    StatementIndentBlock: ({ _nonNL, _EOL }) => _nonNL.chain(newIndent => {
      if (newIndent.length > indent.length) {
        const { Statement } = parserAtIndent(newIndent) as any
        return Statement.sepBy1(_EOL.then(_nonNL.chain(nextIndent => {
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
        })))
      }
      return fail(`Statement block improperly indented\n`
        + `Only indented ${newIndent.length} spaces, needs to be indented `
        + `>${indent.length} spaces`)
    }).trim(_EOL),

    StatementBraceBlock: ({ _nonNL, Statement, StatementIndentBlock }) =>
      s('{').then(alt(
        Statement.sepBy1(s(';').trim(_nonNL)).trim(_nonNL),
        StatementIndentBlock.skip(s(indent)),
      )).skip(s('}')),


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
}

export const parser = parserAtIndent('')
