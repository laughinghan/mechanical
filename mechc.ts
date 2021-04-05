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

// Stages:
//   1. Tokenize with a regex
//   2. Parse into token tree with recursive descent
//   3. Parse into AST with precedence-climbing

// The conventional approach to parsing programming languages is 2 steps:
//   1. The tokenizer (aka lexer or scanner) takes source text as input,
//      and outputs a sequence of tokens.
//   2. The parser takes the sequence of tokens as input, and outputs
//      the AST (abstract syntax tree).
// Typically, they're implemented by writing them in DSLs (domain-specific
// languages) based on Backus-Naur Form, then using a tool like Jison,
// PEG.js, Chevrotain, or Ohm to compile them into executable code. Such
// tools are called "parser generators", or sometimes "compiler compilers".
// Usually one tool generates both steps, and both steps are written in
// similar DSLs, often in the same file, so the distinction just helps
// organize the lexing/parsing code written in the DSL.
//
// In contrast, Mechanical's parser is handwritten directly in executable
// JavaScript (technically, TypeScript). This probably sounds horrifying
// to anyone familiar with a conventional CS education, but actually by
// splitting parsing into 3 steps rather than the usual 2, the implementation
// is reasonably clean:
//
// Stage 1, the tokenizer, is just a regex to turn the source text string
// into a sequence of string tokens.
//
// Stage 2, the token tree parser, only parses the nesting of parentheses/
// brackets/braces and indentation, outputting a sequence of tokens and
// groups which contain a nested sequence of further tokens and groups.
// This is a simple top-down recursive-descent parser.
//
// Stage 3, the AST parser, doesn't need to deal with nesting because that's
// taken care of. It mainly deals with infix operators and their relative
// precedence, using straightforward bottom-up precedence-climbing.
//
// Note that by separating parsing nesting from parsing operator precedence,
// we can use a top-down procedure for one and bottom-up for the other.
//
// This approach has a few benefits over the conventional approach:
//   - Good error messages are easier, because we can ensure we complain
//     at the right time and provide the right information. The generality
//     of parser generators often loses crucial information necessary to
//     provide really excellent error messages.
//   - It allows users to write macros to manipulate the token tree in a
//     forward-compatible way, because the token tree format is more
//     stable than the AST. This is why token trees were invented in the
//     first place (by the Dylan language).
//   - Mechanical's intentionally simple syntax lends itself to being
//     parsed by simple, performant parsing code. Parser generators
//     are generally designed to be able to parse arbitrarily-complex
//     context-free grammars, much of which simple recursive-descent
//     and simple precedence-climbing cannot parse.
//
// Misc. design note:
//   - Conventionally, the tokenizer is responsible for discarding
//     whitespace and comments, and attaching source location info to
//     tokens. In CPython, it also inserts NEWLINE, INDENT, and DEDENT
//     tokens. In Mechanical, the tokenizer is just a regex and doesn't
//     discard any text. The token tree parser handles nested indentation
//     grouping, generating newline tokens as well as unmatched brackets
//     or dedents, and otherwise discarding whitespace and comments,
//     as well as attaching source location info.

// Acknowledgements:
// - this way of structuring compilers was best described by @jneen, who
//   calls our token tree a "skeleton tree":
//     https://tech.trello.com/jeanine-adkisson-skeleton-trees/
// - they were inspired by the "D-expressions" paper, which introduced the
//   skeleton syntax tree as a way for the Dylan language (which has
//   conventional infix syntax) to provide macro facilities as convenient
//   as Lisp (whose macro facilities are so convenient because of its
//   S-expression syntax):
//     https://people.eecs.berkeley.edu/~jrb/Projects/dexprs.pdf
// - since Mechanical's skeleton tree serves a similar role to a conventional
//   tokenizer, I decided to call it a "token tree", like Rust:
//     https://blog.rust-lang.org/2018/12/21/Procedural-Macros-in-Rust-2018.html#whats-inside-a-tokenstream

export namespace TokenTree {
  export type Seq = ReadonlyArray<AnyToken | Group>

  // token variants have to be split up like this for type narrowing to work
  type AnyToken =
      Token<'Ident'>
    | Token<'Punct'>
    | Token<'UnmatchedDelim', 'dedent' | '[' | ']' | '(' | ')' | '{' | '}'>
    | Token<'Numeral'>
    | Token<'StringLiteral'>
    | Token<'FieldFunc'>
    //| Token<'Hashtag'>
  interface Token<T, Val = string> extends Span {
    readonly type: T
    readonly val: Val
  }
  interface Group extends Span {
    readonly type: 'Group'
    readonly delims: 'indent' | '()' | '[]' | '{}'
    readonly nested: Seq
  }
  interface Span {
    readonly i: number
    readonly length: number
  }

  export function tokenize(source: string): string[] {
    // TODO: decimals, E notation, numeric separators
    return source.match(/( *(\/\/[^\n]*)?\n)+| +|\d+[\d_]*|"(\\"|[^"])*"|'(\\'|[^'])*'|\.\w+|\w+|\*\*|==|!=|<=|>=|=>|&&|\|\||\D/g) || []
    // the \D is to match miscellaneous characters, since `.` doesn't
    // match newline-like characters
  }

  type Mismatch = { open?: Token<'Punct' | 'UnmatchedDelim'>, close?: Token<'UnmatchedDelim'> }
  type ParseResult = { tokens: Seq, n: number, srcN: number, mismatches: Mismatch[] }
  function parseSeq(tokens: string[], i: number, srcI: number, indent: number, prevIndent: number, closeDelim: null | ')' | ']' | '}', openI: number, openSrcI: number): ParseResult {
    const seq: Seq[number][] = []
    const mismatches: Mismatch[] = []
    let j = i, srcJ = srcI
    while (j < tokens.length) {
      const token = tokens[j]
      // if this is the close-delim we're waiting for, return without consuming
      if (token === closeDelim) break
      // otherwise, any close-delimiter is an unmatched one
      else if (token === ')' || token === ']' || token === '}') {
        const unmatchedDelim =
          { type: 'UnmatchedDelim', val: token, i: srcJ, length: 1 } as const
        mismatches.push({
          open: isNaN(openI) ? undefined
            : { type: 'Punct', val: tokens[openI], i: openSrcI, length: 1 },
          close: unmatchedDelim })
        seq.push(unmatchedDelim)
        j += 1
        srcJ += 1
        continue
      }
      // if open-delimiter, then recurse
      if (token === '(' || token === '[' || token === '{') {
        const closeDelim = token === '(' ? ')' : token === '[' ? ']' : '}'
        const { tokens: nested, n, srcN, mismatches: newMismatches } =
          parseSeq(tokens, j + 1, srcJ + 1, indent, prevIndent, closeDelim, j, srcJ)
        j += n + 1
        srcJ += srcN + 1
        const lookahead = tokens[j]
        // if recursion ended because it encountered the close-delimiter,
        // then consume it and emit group
        if (lookahead === closeDelim) {
          seq.push({ type: 'Group', delims: token + closeDelim as any, nested,
            i: srcJ - srcN - 1, length: srcN + 2 })
          j += 1
          srcJ += 1
        }
        else { // else, recursion ended on dedent or EOF, so emit
          // unmatched open-delim token and then nested tokens
          const unmatchedDelim =
            { type: 'UnmatchedDelim', val: token, i: srcJ - srcN - 1, length: 1 } as const
          seq.push(unmatchedDelim, ...nested)
          mismatches.push({ open: unmatchedDelim })
        }
        mismatches.push(...newMismatches)
        continue
      }
      const lastCh = token.charAt(token.length - 1)
      // if newline, lookahead at indent of next line
      if (lastCh === '\n') {
        if (j + 1 >= tokens.length) break
        let lookahead = tokens[j + 1]
        let lookaheadIndent =
          lookahead && lookahead.endsWith(' ') ? lookahead.length : 0
        // if unchanged indent level from current, emit newline token
        if (lookaheadIndent === indent) {
          if (j > 0) { // ignore "virtual" newline (see parse())
            seq.push({ type: 'Punct', val: '\n', i: srcJ, length: token.length })
          }
          j += 1 + (lookaheadIndent ? 1 : 0)
          srcJ += token.length + lookaheadIndent
          continue
        }
        // if deeper indent, recurse with new indent
        if (lookaheadIndent > indent) {
          j += 2
          srcJ += token.length + lookaheadIndent
          const { tokens: nested, n, srcN, mismatches: newMismatches } =
            parseSeq(tokens, j, srcJ, lookaheadIndent, indent, closeDelim, openI, openSrcI)

          seq.push({ type: 'Group', delims: 'indent', nested,
            i: srcJ - lookaheadIndent, length: srcN + lookaheadIndent })
          mismatches.push(...newMismatches)
          j += n
          srcJ += srcN
          // if recursion ended in EOF, return
          if (j + 1 >= tokens.length) break
          // otherwise, recursion ended on a dedent, lookahead to it
          lookahead = tokens[j + 1]
          lookaheadIndent =
            lookahead && lookahead.endsWith(' ') ? lookahead.length : 0
          // if closing dedent is exactly the current indent,
          // continue parsing at this indent level
          if (lookaheadIndent === indent) {
            const linebreakLength = tokens[j].length
            j += 1 + (lookaheadIndent ? 1 : 0)
            srcJ += linebreakLength + lookaheadIndent
            continue
          }
        }
        // if dedent to (or past) previous indent, return from current
        // recursion without consuming newline or indent
        if (lookaheadIndent <= prevIndent) break
        // otherwise, must be dedent but only part-of-the-way to previous
        // indent. Emit unmatched dedent token and update current indent
        const unmatchedDedent = { type: 'UnmatchedDelim', val: 'dedent',
          i: srcJ + token.length, length: lookaheadIndent } as const
        seq.push(unmatchedDedent)
        mismatches.push({ close: unmatchedDedent })
        j += 2
        srcJ += token.length + lookaheadIndent
        indent = lookaheadIndent
        continue
      }
      // if ordinary whitespace, skip
      if (lastCh === ' ') {
        j += 1
        srcJ += token.length
        continue
      }
      // otherwise, emit token
      let type: AnyToken['type'] = 'Punct'
      const ch = token.charAt(0), chCode = token.charCodeAt(0)
      if (ch === '_'
        || (0x41 <= chCode && chCode <= 0x5A) // letters A-Z
        || (0x61 <= chCode && chCode <= 0x7A) // letters a-z
      ) {
        type = 'Ident'
      }
      else if (0x30 <= chCode && chCode <= 0x39) { // digits 0-9
        type = 'Numeral'
      }
      else if (token.length > 1) {
        if (ch === '"' || ch === "'") type = 'StringLiteral'
        else if (ch === '.') type = 'FieldFunc'
      }
      seq.push({ type, val: token, i: srcJ, length: token.length })
      j += 1
      srcJ += token.length
    }
    return { tokens: seq, n: j - i, srcN: srcJ - srcI, mismatches }
  }

  export function parse(source: string): { tokens: Seq, mismatches: Mismatch[] } {
    // prefix with "virtual" newline to handle leading indent
    return parseSeq(['\n'].concat(tokenize(source)), 0, -1, 0, NaN, null, NaN, NaN)
  }
}

export namespace AST {
  export type Expression = PrimaryExpr | FieldAccessExpr | CallExpr | UnaryExpr
    | BinaryExpr | CompareChainExpr | CondExpr | ArrowFunc

  export type PrimaryExpr = Numeral | StringLiteral | FieldFunc | Variable
    | ArrayLiteral | RecordLiteral
  export interface Numeral {
    readonly type: 'Numeral'
    readonly val: string
  }
  export interface StringLiteral {
    readonly type: 'StringLiteral'
    readonly val: string
  }
  export interface FieldFunc {
    readonly type: 'FieldFunc'
    readonly val: string
  }
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

  type ParseError = { i: number, length?: number, code: string, msg: string }
  export function parseExpr(tokens: TokenTree.Seq, i: number): {
    expr?: AST.Expression, n: number, errors: ParseError[]
  } {
    const result = (expr: AST.Expression, errors: ParseError[] = [], n = 1) =>
      ({ expr, n, errors })
    const token = tokens[i]
    switch (token.type) {
      case 'Ident':
        return result({ type: 'Variable', name: token.val })
      case 'Numeral':
      case 'StringLiteral':
      case 'FieldFunc':
        delete (token as any).i
        delete (token as any).length
        // ^ temporary, for tests, TODO: remove
        return result(token)
      case 'Group':
        switch (token.delims) {
          case '[]':
            // accumulate exprs & errors by looping over nested token trees
            const exprs: Expression[] = []
            const errors: ParseError[] = []
            let nestedI = 0
            while (nestedI < token.nested.length) {
              // parse item
              const item = parseExpr(token.nested, nestedI)
              nestedI += item.n
              errors.push(...item.errors)
              if (item.expr) exprs.push(item.expr)

              // if no trailing comma, no problem
              if (nestedI >= token.nested.length) break
              // other than that, consume comma after each item
              const next = token.nested[nestedI]
              if (next.type === 'Punct' && next.val === ',') {
                nestedI += 1
              } else {
                errors.push({ i: next.i, code: 'array_comma',
                  msg: 'Parsing array, just finished parsing array item, expected comma, got: ' + JSON.stringify(next) })
                break
              }
            }
            return result({ type: 'ArrayLiteral', exprs }, errors)
          default:
            throw `parseExpr() of Group with delims '${token.delims}' not yet implemented`
        }
      default:
        throw `parseExpr() of token type '${token.type}' not yet implemented`
    }
  }
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
    .map(val => ({ type: 'Numeral', val } as const))
  const FieldFunc = s('.').then(Identifier)
    .map(name => ({ type: 'FieldFunc', val: '.' + name } as const))
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
      return succeed({ type: 'StringLiteral', val: dedented } as const)
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

const JS_RESERVED_WORDS: {readonly [kw: string]: true} = {}
for (const word of `abstract arguments await boolean break byte case catch char
class const continue debugger default delete do double else enum export extends
false final finally float for function get goto if import implements in int
interface instanceof let long native new null package private protected public
return set short static super switch synchronized this throw throws transient
true try typeof undefined var void volatile while with yield`.split(/\s+/)
) (JS_RESERVED_WORDS as any)[word] = true

function codegenPrecedence(expr: AST.Expression): number {
  if (typeof expr === 'string') return 0
  switch (expr.type) {
    case 'Numeral':
    case 'StringLiteral':
    case 'FieldFunc':
    case 'Variable':
    case 'ArrayLiteral':
    case 'RecordLiteral':
    case 'FieldAccessExpr':
    case 'CallExpr':
      return 0 // primary expressions
    case 'UnaryExpr':
      return 1 // unary expressions
    case 'BinaryExpr':
      switch (expr.op) {
        case '**':
          return 2 // exponentiation
        case '*':
        case '/':
        case '%':
          return 3 // multiplication
        case '+':
        case '-':
          return 4 // addition
        case '==':
        case '!=':
        case '<':
        case '>':
        case '<=':
        case '>=':
          return 5 // comparisons
        case '&&':
        case '||':
          return 6 // logical operators
      }
    case 'CompareChainExpr':
      return 6 // comparisons joined by &&'s
    case 'CondExpr':
      return 7
    case 'ArrowFunc':
      return 8
  }
}

export function codegenExpr(ctx: Context, expr: AST.Expression): string {
  function parenthesize(subExpr: AST.Expression, strict?: boolean) {
    if (!strict && codegenPrecedence(subExpr) === codegenPrecedence(expr)) {
      return codegenExpr(ctx, subExpr)
    }
    else if (codegenPrecedence(subExpr) < codegenPrecedence(expr)) {
      return codegenExpr(ctx, subExpr)
    }
    return '(' + codegenExpr(ctx, subExpr) + ')'
  }
  switch (expr.type) {
    case 'Numeral':
    case 'StringLiteral':
      // this "just works" for booleans, numerals, and single-line string literals
      // TODO: multi-line string literals
      return expr.val
    case 'FieldFunc':
      return `(record => record${expr.val})`
    case 'Variable':
      if (!(expr.name in ctx.scope)) throw new Error(`Unbound variable: ${expr.name}`)
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
    case 'FieldAccessExpr':
      return parenthesize(expr.record) + '.' + expr.fieldName
    case 'CallExpr':
      if (expr.contextArg) {
        return parenthesize(expr.func) + '('
          + codegenExpr(ctx, expr.contextArg)
          + expr.args.map(({arg}) => ', ' + codegenExpr(ctx, arg)).join('')
          + ')'
      }
      return parenthesize(expr.func)
        + `(${expr.args.map(({arg}) => codegenExpr(ctx, arg)).join(', ')})`
    case 'UnaryExpr':
      return expr.op + parenthesize(expr.arg)
    case 'BinaryExpr': {
      const { left, op, right } = expr
      const paddedOp =
        (op === '**' || op === '*' || op === '/') ? op
        : (op === '==' || op === '!=') ? ` ${op}= `
        : ` ${op} `
      switch (op) {
        case '*': // fully associative
        case '+':
          return parenthesize(left) + paddedOp + parenthesize(right)

        case '/': // left-associative
        case '%':
        case '-':
          return parenthesize(left) + paddedOp + parenthesize(right, true)

        case '**': // non-associative
        case '==':
        case '!=':
        case '<':
        case '>':
        case '<=':
        case '>=':
          return parenthesize(left, true) + paddedOp + parenthesize(right, true)
      }
      // && and || are associative but "exclusive precedence", meaning
      // neither has higher precedence than the other. We do this even
      // though in other ALGOL descendents it's pretty much universal that
      // && has higher precedence than || (because it's mathematically
      // multiplication, and || is addition), because I think it doesn't
      // _read_ as having higher precedence; I think combined && and || is
      // much more readable with parens. And unlike arithmetic, you never
      // need nested parens thanks to de Morgan's laws.
      // TODO: open a discussion ticket
      const leftOpIsOr  = left.type  === 'BinaryExpr' && left.op  === '||'
      const rightOpIsOr = right.type === 'BinaryExpr' && right.op === '||'

      // between booleans, !== is xor. So strict parenthesize:
      //   - if this op is && and the subexpr op is ||
      //   - or if this op is || , and the subexpr op isn't ||
      const leftExpr  = parenthesize(left,  leftOpIsOr  !== (op === '||'))
      const rightExpr = parenthesize(right, rightOpIsOr !== (op === '||'))
      return leftExpr + paddedOp + rightExpr
    }
    case 'CompareChainExpr':
      return expr.chain.map(binop => codegenExpr(ctx, binop)).join(' && ')
    case 'CondExpr':
      return parenthesize(expr.test, true)
        + ' ? ' + parenthesize(expr.ifYes)
        + ' : ' + parenthesize(expr.ifNo)
    case 'ArrowFunc':
      const scope = { ...ctx.scope }
      const params = expr.params.slice() // array copy
      for (let i = 0; i < params.length; i += 1) {
        const param = params[i]
        if (JS_RESERVED_WORDS[param]) {
          scope[param] = params[i] = param + '_'
        } else {
          scope[param] = param
        }
      }
      const paramsStr = params.length > 1 ? `(${params.join(', ')})` : params[0]

      const { body } = expr
      if (body instanceof Array) {
        let i = 0
        const newTemp = () => '_' + (i += 1)
        const indent = ctx.indent + '  '
        return paramsStr + ' => {\n' + body.map(stmt => {
          const { code, newVar } = codegenStmt({ indent, scope }, stmt)
          if (newVar) scope[newVar.replace(/_$/, '')] = newVar
          return code
        }).join('') + ctx.indent + '}'
      }
      const bodyAsExpr = body as AST.Expression // this is only necessary because
        // TypeScript's type narrowing incorrectly considers `readonly AST.Statement[]`
        // to be disjoint from `instanceof Array` (when in fact it's obviously
        // a subtype). As of this writing this is fixed in TypeScript's Nightly
        // release, I think by this PR:
        //   https://github.com/microsoft/TypeScript/pull/39258#issuecomment-720729851
        // So when that ships, we should be able to remove this line.
      const bodyStr = codegenExpr({ ...ctx, scope }, bodyAsExpr)
      return paramsStr + ' => '
        + (bodyStr.startsWith('{') ? `(${bodyStr})` : bodyStr)
  }
}

export function codegenStmt(ctx: Context, stmt: AST.Statement): { code: string, newVar?: string } {
  let code: string, newVar: string | undefined = undefined
  switch (stmt.type) {
    case 'ReturnStmt':
      code = 'return ' + codegenExpr(ctx, stmt.expr)
      break
    case 'EmitStmt':
      // TODO: somehow check this is in an event emitter
      code = `$emit(${codegenExpr(ctx, stmt.expr)})`
      break
    case 'LetStmt':
      newVar = stmt.varName
      if (JS_RESERVED_WORDS[newVar]) newVar += '_'
      code = `const ${newVar} = ${codegenExpr(ctx, stmt.expr)}`
      break
    case 'DoStmt':
      code = codegenExpr(ctx, stmt.expr) + '()'
      break
    default:
      throw 'Unexpected or not-yet-implemented statement type: ' + stmt.type
  }
  code = ctx.indent + code + ';\n'
  return { code, newVar }
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
    const { code, newVar } = codegenStmt(topLevelContext, statement)
    js += code
    if (newVar) topLevelContext.scope[newVar.replace(/_$/, '')] = newVar
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
