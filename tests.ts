import 'mocha'
import assert from 'assert'
import fc from 'fast-check'
import { exec } from 'child_process'
import { Readable } from 'stream'
import { Failure } from 'parsimmon'

import { TokenTree, AST, parserAtIndent, parser, TopLevel, ProgramParser, Types, compile, codegenExpr, codegenStmt } from './mechc'

import { arb_nontrivial_type, arb_type_pairs, arb_similar_types, perturb_type, isWellFormed } from './test_helpers'

function pre(condition: boolean): asserts condition {
  // workaround for extremely confusing TypeScript error message:
  //
  //     Assertions require every name in the call target to be declared with an explicit type annotation. (TS2775)
  //
  // I tried to declare fc.pre or directly-imported pre (meaning
  // `import {pre} from 'fast-check'`) with with an explicit type
  // annotation, still kept getting that error message.
  //
  // See also https://github.com/microsoft/TypeScript/issues/36931#issuecomment-633659882

  fc.pre(condition)
}

suite('TokenTree parsing', () => {
  suite('tokenize()', () => {
    test('basic', () => {
      const observed = TokenTree.tokenize(
          '// count how many times button was clicked\n'
        + 'State counter = 2\n'
        + '\n'
        + 'When btnClick:   \n'
        + '    \n'
        + '    // when button is clicked, square counter\n'
        + '    Change counter to   counter**2\n'
        + '\n'
      )
      const expected = [
        '// count how many times button was clicked\n',
        'State', ' ', 'counter', ' ', '=', ' ', '2', '\n\n',
        'When', ' ', 'btnClick', ':',
        '   \n    \n    // when button is clicked, square counter\n',
        '    ', 'Change', ' ', 'counter', ' ', 'to', '   ', 'counter', '**',
        '2', '\n\n',
      ]
      assert.deepStrictEqual(observed, expected)
    })
    test('numerals vs identifiers containing digits', () => {
      const observed = TokenTree.tokenize('x1+1-2x')
      const expected = ['x1', '+', '1', '-', '2', 'x']
      assert.deepStrictEqual(observed, expected)
    })
    test('lossless (never drops characters)', () => {
      fc.assert(fc.property(fc.asciiString(), str => {
        assert.strictEqual(str, TokenTree.tokenize(str).join(''))
      }))
      fc.assert(fc.property(fc.string16bits(), str => {
        assert.strictEqual(str, TokenTree.tokenize(str).join(''))
      }))
    })
  })
  suite('parse()', () => {
    function parseAndCheck(source: string) {
      const parseResult = TokenTree.parse(source)
      const unmatchedDelims = walk(parseResult.tokens)

      const N = Math.max(unmatchedDelims.length, parseResult.mismatches.length)
      for (let i = 0; i < N; i += 1) {
        const obsUnmatched = parseResult.mismatches[i]
        const {open, close} = obsUnmatched
        if (open) {
          assert.strictEqual(open.length, 1)
          assert.strictEqual(source.slice(open.i, open.i + 1), open.val)
        }
        if (close) {
          if (close.val === 'dedent') {
            assert(/^ +$/.test(source.slice(close.i, close.i + close.length)))
            continue
          } else {
            assert.strictEqual(close.length, 1)
            assert.strictEqual(source.slice(close.i, close.i + 1), close.val)
          }
        }

        const foundUnmatched = unmatchedDelims[i]
        if (/[([{]/.test(foundUnmatched.val)) {
          assert.deepStrictEqual(obsUnmatched.open,  foundUnmatched)
        } else {
          assert.deepStrictEqual(obsUnmatched.close, foundUnmatched)
        }
      }
      return parseResult

      function walk(tokens: TokenTree.Seq) {
        const unmatchedDelims: any[] = []
        for (const t of tokens) {
          if (t.type === 'Group') {
            if (t.delims === 'indent') {
              assert(t.nested.length > 0)
              const indent = source.slice(t.i, t.nested[0].i)
              assert(/^ +$/.test(indent))
              const lastToken = t.nested[t.nested.length - 1]
              assert.strictEqual(lastToken.i + lastToken.length, t.i + t.length)

              unmatchedDelims.push(...walk(t.nested))
            } else {
              assert.strictEqual(t.delims.charAt(0), source.slice(t.i, t.i + 1))
              assert.strictEqual(t.delims.charAt(1), source.slice(t.i + t.length - 1, t.i + t.length))
              if (t.nested.length) {
                const leadingSpace = source.slice(t.i + 1, t.nested[0].i)
                assert(/^[ \n]*$/.test(leadingSpace))
                const lastToken = t.nested[t.nested.length - 1]
                const trailingSpace = source.slice(lastToken.i + lastToken.length, t.i + t.length - 1)
                assert(/^[ \n]*$/.test(trailingSpace))

                unmatchedDelims.push(...walk(t.nested))
              } else {
                const emptySpace = source.slice(t.i + 1, t.i + t.length - 1)
                assert(/^[ \n]*$/.test(emptySpace))
              }
            }
          } else {
            if (t.val === '\n') {
              assert(/ *(\/\/[^\n]*)?\n/.test(source.slice(t.i, t.i + t.length)))
            }
            else if (t.type === 'UnmatchedDelim' && t.val === 'dedent') {
              assert(/^ +$/.test(source.slice(t.i, t.i + t.length)))
            }
            else {
              assert.strictEqual(source.slice(t.i, t.i + t.length), t.val)
            }

            if (t.type === 'UnmatchedDelim') unmatchedDelims.push(t)
          }
        }
        return unmatchedDelims
      }
    }
    function cleanup(obj: any) {
      delete obj.n
      delete obj.srcN
      obj.tokens.forEach(cleanupSpanInfo)
      obj.mismatches.forEach((m: any) => {
        if (m.open)  cleanupSpanInfo(m.open)
        else delete m.open
        if (m.close) cleanupSpanInfo(m.close)
        else delete m.close
      })
    }
    function cleanupSpanInfo(token: any) {
      delete token.i
      delete token.length
      if (token.type === 'Group') {
        token.nested.forEach(cleanupSpanInfo)
      }
    }

    test('basic', () => {
      const observed = parseAndCheck(
          '// count how many times button was clicked\n'
        + 'State counter = 2\n'     // normal line-break
        + '\n'
        + 'When btnClick:   \n'     // indent
        + '    \n'
        + '    // when button is clicked, square counter\n'
        + '  Let y = x.foo()\n'     // normal line-break
        + '  Let f = x =>\n'        // indent
        + '    5*x**2 + 10*x + 5\n' // dedent
        + '  Let z = 2*(x\n'        // open-paren ( and indent
        + '             + 1)\n'     // close-paren ), then indent (relative to Let)
        + '          + 4\n'         // dedent
        + '  Let obj = {\n'         // open-brace { and indent
        + '       foo: "foo",\n'    // a normal line-break
        + '       bar: "bar",\n'    // unmatched dedent
        + '      asdf: "asdf",\n'   // indent
        + '         a: "a",\n'      // proper dedent then unmatched dedent
        + '     xyzzy: "xyzzy",\n'  // dedent
        + '  }\n'                   // close-brace } and dedent (implicit)
      )
      const expected = {
        tokens: [
          { type: 'Punct', val: '\n' },
          { type: 'Ident', val: 'State' },
          { type: 'Ident', val: 'counter' },
          { type: 'Punct', val: '=' },
          { type: 'Numeral', val: '2' },
          { type: 'Punct', val: '\n' },
          { type: 'Ident', val: 'When' },
          { type: 'Ident', val: 'btnClick' },
          { type: 'Punct', val: ':' },
          { type: 'Group', delims: 'indent', nested: [
            { type: 'Ident', val: 'Let' },
            { type: 'Ident', val: 'y' },
            { type: 'Punct', val: '=' },
            { type: 'Ident', val: 'x' },
            { type: 'FieldFunc', val: '.foo' },
            { type: 'Group', delims: '()', nested: [] },
            { type: 'Punct', val: '\n' },
            { type: 'Ident', val: 'Let' },
            { type: 'Ident', val: 'f' },
            { type: 'Punct', val: '=' },
            { type: 'Ident', val: 'x' },
            { type: 'Punct', val: '=>' },
            { type: 'Group', delims: 'indent', nested: [
              { type: 'Numeral', val: '5' },
              { type: 'Punct', val: '*' },
              { type: 'Ident', val: 'x' },
              { type: 'Punct', val: '**' },
              { type: 'Numeral', val: '2' },
              { type: 'Punct', val: '+' },
              { type: 'Numeral', val: '10' },
              { type: 'Punct', val: '*' },
              { type: 'Ident', val: 'x' },
              { type: 'Punct', val: '+' },
              { type: 'Numeral', val: '5' },
            ]},
            { type: 'Ident', val: 'Let' },
            { type: 'Ident', val: 'z' },
            { type: 'Punct', val: '=' },
            { type: 'Numeral', val: '2' },
            { type: 'Punct', val: '*' },
            { type: 'Group', delims: '()', nested: [
              { type: 'Ident', val: 'x' },
              { type: 'Group', delims: 'indent', nested: [
                { type: 'Punct', val: '+' },
                { type: 'Numeral', val: '1' },
              ]},
            ]},
            { type: 'Group', delims: 'indent', nested: [
              { type: 'Punct', val: '+' },
              { type: 'Numeral', val: '4' },
            ]},
            { type: 'Ident', val: 'Let' },
            { type: 'Ident', val: 'obj' },
            { type: 'Punct', val: '=' },
            { type: 'Group', delims: '{}', nested: [
              { type: 'Group', delims: 'indent', nested: [
                { type: 'Ident', val: 'foo' },
                { type: 'Punct', val: ':' },
                { type: 'StringLiteral', val: '"foo"'},
                { type: 'Punct', val: ',' },
                { type: 'Punct', val: '\n' },
                { type: 'Ident', val: 'bar' },
                { type: 'Punct', val: ':' },
                { type: 'StringLiteral', val: '"bar"'},
                { type: 'Punct', val: ',' },
                { type: 'UnmatchedDelim', val: 'dedent' },
                { type: 'Ident', val: 'asdf' },
                { type: 'Punct', val: ':' },
                { type: 'StringLiteral', val: '"asdf"'},
                { type: 'Punct', val: ',' },
                { type: 'Group', delims: 'indent', nested: [
                  { type: 'Ident', val: 'a' },
                  { type: 'Punct', val: ':' },
                  { type: 'StringLiteral', val: '"a"'},
                  { type: 'Punct', val: ',' },
                ]},
                { type: 'UnmatchedDelim', val: 'dedent' },
                { type: 'Ident', val: 'xyzzy' },
                { type: 'Punct', val: ':' },
                { type: 'StringLiteral', val: '"xyzzy"'},
                { type: 'Punct', val: ',' },
              ]},
            ]},
          ]},
        ],
        mismatches: [
          { close: { type: 'UnmatchedDelim', val: 'dedent' } },
          { close: { type: 'UnmatchedDelim', val: 'dedent' } },
        ],
      }
      cleanup(observed)
      assert.deepStrictEqual(observed, expected)
    })
    test('leading indent', () => {
      const observed = parseAndCheck(
          '  foo\n'
        + '  bar\n'
        + '    qux\n'
        + 'asdf\n')
      const expected = {
        tokens: [
          { type: 'Group', delims: 'indent', nested: [
            { type: 'Ident', val: 'foo' },
            { type: 'Punct', val: '\n' },
            { type: 'Ident', val: 'bar' },
            { type: 'Group', delims: 'indent', nested: [
              { type: 'Ident', val: 'qux' },
            ]},
          ]},
          { type: 'Ident', val: 'asdf' },
        ],
        mismatches: [],
      }
      cleanup(observed)
      assert.deepStrictEqual(observed, expected)
    })
    test('spacing in and around delimiter-groups', () => {
      const observed = parseAndCheck('(1 + ( -2 * 3) + [5  ])')
      const expected = {
        tokens: [
          { type: 'Group', delims: '()', nested: [
            { type: 'Numeral', val: '1' },
            { type: 'Punct', val: '+' },
            { type: 'Group', delims: '()', nested: [
              { type: 'Punct', val: '-' },
              { type: 'Numeral', val: '2' },
              { type: 'Punct', val: '*' },
              { type: 'Numeral', val: '3' },
            ]},
            { type: 'Punct', val: '+' },
            { type: 'Group', delims: '[]', nested: [
              { type: 'Numeral', val: '5' },
            ]},
          ]},
        ],
        mismatches: [],
      }
      cleanup(observed)
      assert.deepStrictEqual(observed, expected)
    })
    test('unmatched delimiters in indent group', () => {
      const observed = parseAndCheck(
          'Let y =\n'
        + '  )\n'
        + '  (\n'
        + 'Do foo\n'
      )
      const expected = {
        tokens: [
          { type: 'Ident', val: 'Let' },
          { type: 'Ident', val: 'y' },
          { type: 'Punct', val: '=' },
          { type: 'Group', delims: 'indent', nested: [
            { type: 'UnmatchedDelim', val: ')' },
            { type: 'Punct', val: '\n' },
            { type: 'UnmatchedDelim', val: '(' },
          ]},
          { type: 'Ident', val: 'Do' },
          { type: 'Ident', val: 'foo' },
        ],
        mismatches: [
          { close: { type: 'UnmatchedDelim', val: ')' } },
          { open:  { type: 'UnmatchedDelim', val: '(' } },
        ],
      }
      cleanup(observed)
      assert.deepStrictEqual(observed, expected)
    })
    test('dedent cuts short delimited group', () => {
      const observed = parseAndCheck(
          '  Let y = [\n'
        + '    1,\n'
        + '    2,\n'
        + '3,\n'
        + '  ]'
      )
      const expected = {
        tokens: [
          { type: 'Group', delims: 'indent', nested: [
            { type: 'Ident', val: 'Let' },
            { type: 'Ident', val: 'y' },
            { type: 'Punct', val: '=' },
            { type: 'UnmatchedDelim', val: '[' },
            { type: 'Group', delims: 'indent', nested: [
              { type: 'Numeral', val: '1' },
              { type: 'Punct', val: ',' },
              { type: 'Punct', val: '\n' },
              { type: 'Numeral', val: '2' },
              { type: 'Punct', val: ',' },
            ]},
          ]},
          { type: 'Numeral', val: '3' },
          { type: 'Punct', val: ',' },
          { type: 'Group', delims: 'indent', nested: [
            { type: 'UnmatchedDelim', val: ']' },
          ]},
        ],
        mismatches: [
          { open:  { type: 'UnmatchedDelim', val: '[' } },
          { close: { type: 'UnmatchedDelim', val: ']' } },
        ],
      }
      cleanup(observed)
      assert.deepStrictEqual(observed, expected)
    })
    test('unmatched close-delimiter in delimited group', () => {
      const observed = parseAndCheck('Let y = (x + ] + 1)\n')
      const expected = {
        tokens: [
          { type: 'Ident', val: 'Let' },
          { type: 'Ident', val: 'y' },
          { type: 'Punct', val: '=' },
          { type: 'Group', delims: '()', nested: [
            { type: 'Ident', val: 'x' },
            { type: 'Punct', val: '+' },
            { type: 'UnmatchedDelim', val: ']' },
            { type: 'Punct', val: '+' },
            { type: 'Numeral', val: '1' },
          ]},
        ],
        mismatches: [
          {
            open: { type: 'Punct', val: '(' },
            close: { type: 'UnmatchedDelim', val: ']' },
          },
        ],
      }
      cleanup(observed)
      assert.deepStrictEqual(observed, expected)
    })
  })
})

type Term = string | AST.Expression
const Term = <E>(val: E): Exclude<E, string> | AST.Expression =>
  typeof val !== 'string' ? val as Exclude<E, string> // too bad TypeScript doesn't type-narrow to this automatically
    : /^\d/.test(val)     ? { type: 'Numeral',       val } as const
    : /^['"]/.test(val)   ? { type: 'StringLiteral', val } as const
    : val.startsWith('.') ? { type: 'FieldFunc',     val } as const
    : { type: 'Variable', name: val } as const

const ArrayLiteral = (exprs: Term[]) =>
  ({ type: 'ArrayLiteral', exprs: exprs.map(Term) } as const)
const RecordLiteral = (obj: {[key: string]: Term}) => ({
  type: 'RecordLiteral',
  pairs: Object.keys(obj).map(key => ({ key, val: Term(obj[key]) })),
} as const)

const FieldAccess = (record: Term, fieldName: string) =>
  ({ type: 'FieldAccessExpr', record: Term(record), fieldName } as const)

const FnCall = (func: Term, arg: Term) =>
  MethodCall(null, func, arg)
const FnCallN = (func: Term, args: {[label: string]: Term}) =>
  MethodCallN(null, func, args)
const MethodCall = (ctx: Term | null, func: Term, arg?: Term) =>
  ({ type: 'CallExpr', contextArg: Term(ctx), func: Term(func),
    args: (arg ? [{ label: null, arg: Term(arg) }] : []) } as const)
const MethodCallN = (ctx: Term | null, func: Term, args: {[label: string]: Term}) =>
  ({ type: 'CallExpr', contextArg: Term(ctx), func: Term(func),
    args: Object.keys(args).map(label =>
      ({ label, arg: Term(args[label]) })) } as const)

const Unop = (op: AST.UnaryExpr['op'], arg: Term) =>
  ({ type: 'UnaryExpr', op, arg: Term(arg) } as const)
const Binop = (left: Term, op: AST.BinaryExpr['op'], right: Term) =>
  ({ type: 'BinaryExpr', op, left: Term(left), right: Term(right) } as const)
const CompareChain = (...chain: AST.BinaryExpr[]) =>
  ({ type: 'CompareChainExpr', chain } as const)
const CondExpr = (test: Term, ifYes: Term, ifNo: Term) =>
  ({ type: 'CondExpr', test: Term(test),
    ifYes: Term(ifYes), ifNo: Term(ifNo) } as const)
const ArrowFunc = (params: string, body: Term | AST.Statement[]) =>
  ({ type: 'ArrowFunc', params: params.split(' '), body: Term(body) } as const)

const LetStmt = (varName: string, expr: Term) =>
  ({ type: 'LetStmt', varName, expr: Term(expr) } as const)
const ChangeStmt = (varName: string, expr: Term) =>
  ({ type: 'ChangeStmt', varName, expr: Term(expr) } as const)
const ReturnStmt = (expr: Term) =>
  ({ type: 'ReturnStmt', expr: Term(expr) } as const)
const EmitStmt = (expr: Term) =>
  ({ type: 'EmitStmt', expr: Term(expr) } as const)
const DoStmt = (expr: Term) =>
  ({ type: 'DoStmt', expr: Term(expr) } as const)
const AfterGotStmt = (vars: string) =>
  ({ type: 'AfterGotStmt', vars: vars.split(' ') } as const)

const StateDecl = (varName: string, expr: Term) =>
  ({ type: 'StateDecl', varName, expr: Term(expr) } as const)
const WhenDecl = (event: Term, varName: string | null, body: AST.Statement[]) =>
  ({ type: 'WhenDecl', event: Term(event), varName, body } as const)

suite('AST parsing', () => {
  function tryParseExpr(source: string): AST.Expression {
    const result = AST.parseExpr(TokenTree.parse(source).tokens, 0)
    assert(result.expr)
    return result.expr
  }
  suite('primary exprs', () => {
    suite('identifiers', () => {
      test('basic this_is_valid', () => {
        const observed = tryParseExpr('this_is_valid')
        const expected = Term('this_is_valid')
        assert.deepStrictEqual(observed, expected)
      })
      test('invalid _foo, foo__bar, foo_, $foo', () => {
        assert(!parser.Expression.parse('_foo').status)
        assert(!parser.Expression.parse('foo__bar').status)
        assert(!parser.Expression.parse('foo_').status)
        assert(!parser.Expression.parse('$foo').status)
      })
    })
    suite('numerals', () => {
      test('basic nonnegative integers', () => {
        assert.deepStrictEqual(tryParseExpr('0'),   Term('0'))
        assert.deepStrictEqual(tryParseExpr('123'), Term('123'))
      })
      // TODO: decimals, exponential notation, hexadecimals?
    })
    suite('field access functions', () => {
      test('basic .field_name', () => {
        const observed = tryParseExpr('.field_name')
        const expected = Term('.field_name')
        assert.deepStrictEqual(observed, expected)
      })
      test('no space allowed after dot ". field_name"', () => {
        assert(!parser.Expression.parse('. field_name').status)
      })
    })
    suite('string literals', () => {
      test('basic "asdf"', () => {
        const observed = tryParseExpr('"asdf"')
        const expected = Term('"asdf"')
        assert.deepStrictEqual(observed, expected)
      })
      test("basic 'asdf'", () => {
        const observed = tryParseExpr("'asdf'")
        const expected = Term("'asdf'")
        assert.deepStrictEqual(observed, expected)
      })
      test('basic ""', () => {
        const observed = tryParseExpr('""')
        const expected = Term('""')
        assert.deepStrictEqual(observed, expected)
      })
      test("basic ''", () => {
        const observed = tryParseExpr("''")
        const expected = Term("''")
        assert.deepStrictEqual(observed, expected)
      })
      test('escaping double-quotes', () => {
        const observed = tryParseExpr('"you could call it \\"weird\\", I guess"')
        const expected = Term('"you could call it \\"weird\\", I guess"')
        assert.deepStrictEqual(observed, expected)
      })
      test('escaping single-quotes', () => {
        const observed = tryParseExpr("'you could call it \\'weird\\', I guess'")
        const expected = Term("'you could call it \\'weird\\', I guess'")
        assert.deepStrictEqual(observed, expected)
      })
      test('multiline double-quotes', () => {
        const observed = tryParseExpr('"first line\nsecond line"')
        const expected = Term('"first line\nsecond line"')
        assert.deepStrictEqual(observed, expected)
      })
      test('multiline single-quotes', () => {
        const observed = tryParseExpr("'first line\nsecond line'")
        const expected = Term("'first line\nsecond line'")
        assert.deepStrictEqual(observed, expected)
      })
      test('indented multiline double-quotes', () => {
        const indentedParser = parserAtIndent('  ')
        const observed = indentedParser.Expression.tryParse('"first\n  second\n    third\n  fourth"')
        const expected = Term('"first\nsecond\n  third\nfourth"')
        assert.deepStrictEqual(observed, expected)
      })
      test('indented multiline single-quotes', () => {
        const indentedParser = parserAtIndent('  ')
        const observed = indentedParser.Expression.tryParse("'first\n  second\n    third\n  fourth'")
        const expected = Term("'first\nsecond\n  third\nfourth'")
        assert.deepStrictEqual(observed, expected)
      })
      test('multiline string requires indent', () => {
        const indentedParser = parserAtIndent('    ')
        const result = indentedParser.Expression.parse('"first\n   second"')
        assert(!result.status)
        assert.strictEqual((result as Failure).index.offset, 10)
      })
      test('multiline string inside arrow func integration test', () => {
        const indentedParser = parserAtIndent('    ')
        const observed = indentedParser.Expression.tryParse('x => {\n'
          + '        Let y = "first\n'
          + '          second\n'
          + '        third"\n'
          + '        Return y\n'
          + '    }')
        const expected = ArrowFunc('x', [
          LetStmt('y', '"first\n  second\nthird"'),
          ReturnStmt('y'),
        ])
        assert.deepStrictEqual(observed, expected)
      })
      test('mismatched quotes', () => {
        assert(!parser.Expression.parse('"text').status)
        assert(!parser.Expression.parse('"text \\"something" else"').status)
        assert(!parser.Expression.parse("'text").status)
        assert(!parser.Expression.parse("'text \\'something' else'").status)
      })
    })
    suite('array literals', () => {
      test('basic [1,2,3]', () => {
        const observed = tryParseExpr('[ 1, 2, 3 ]')
        const expected = ArrayLiteral([ '1', '2', '3' ])
        assert.deepStrictEqual(observed, expected)
      })
      test('basic empty []', () => {
        const observed = tryParseExpr('[]')
        const expected = ArrayLiteral([])
        assert.deepStrictEqual(observed, expected)
      })
      test('trailing comma [1,2,]', () => {
        const observed = tryParseExpr('[ 1, 2, ]')
        const expected = ArrayLiteral([ '1', '2' ])
        assert.deepStrictEqual(observed, expected)
      })
      test('newlines [1,2,]', () => {
        const observed = tryParseExpr(`[
          1,
          2,
        ]`)
        const expected = ArrayLiteral([ '1', '2' ])
        assert.deepStrictEqual(observed, expected)
      })
      test('comma-first', () => {
        const observed = tryParseExpr(
          `[ 1
           , 2
           , 3
           ]`)
        const expected = ArrayLiteral([ '1', '2', '3' ])
        assert.deepStrictEqual(observed, expected)
      })
      test('weird indentation', () => {
        const observed = tryParseExpr(`[
          1,
          2
            ,
           3
         ,
          4,
        ]`)
        const expected = ArrayLiteral([ '1', '2', '3', '4' ])
        assert.deepStrictEqual(observed, expected)
      })
      test('invalid (holes) [1,,2]', () => {
        assert(!parser.Expression.parse('[ 1, , 2 ]').status)
        assert(!parser.Expression.parse('[ 1, 2,, ]').status)
      })
      test('invalid single comma [,]', () => {
        assert(!parser.Expression.parse('[,]').status)
        assert(!parser.Expression.parse('[ , ]').status)
      })
      test('invalid (missing comma) [1 2]', () => {
        assert(!parser.Expression.parse('[1 2]').status)
        assert(!parser.Expression.parse('[[1] 2]').status)
        assert(!parser.Expression.parse('[1 [2]]').status)
      })
      test('invalid mismatched brackets', () => {
        assert(!parser.Expression.parse('[1, 2').status)
        assert(!parser.Expression.parse('1, 2]').status)
        assert(!parser.Expression.parse('[[1, 2]').status)
        assert(!parser.Expression.parse('[[1], 2').status)
      })
    })
    suite('record literals', () => {
      test('basic {a: 1, b:2}', () => {
        const observed = parser.Expression.tryParse('{a: 1, b: 2}')
        const expected = RecordLiteral({ a: '1', b: '2' })
        assert.deepStrictEqual(observed, expected)
      })
      test('trailing comma {a: 1, b:2,}', () => {
        const observed = parser.Expression.tryParse('{a: 1, b: 2,}')
        const expected = RecordLiteral({ a: '1', b: '2' })
        assert.deepStrictEqual(observed, expected)
      })
      test('empty record {}', () => {
        const observed = parser.Expression.tryParse('{}')
        const expected = RecordLiteral({})
        assert.deepStrictEqual(observed, expected)
      })
      test('record field name punning {a}', () => {
        const observed = parser.Expression.tryParse('{a}')
        const expected = RecordLiteral({ a: 'a' })
        assert.deepStrictEqual(observed, expected)
      })
      test('mixed obj { a: 1, b, c, }', () => {
        const observed = parser.Expression.tryParse('{ a: 1, b, c, }')
        const expected = RecordLiteral({ a: '1', b: 'b', c: 'c' })
        assert.deepStrictEqual(observed, expected)
      })
      test('invalid without comma {a b}', () => {
        assert(!parser.Expression.parse('{a b}').status)
      })
      test('allow expressions in values { a: 1+1, ... }', () => {
        const observed = parser.Expression.tryParse(`{
          a: 1+1,
          b: x && y ? z : t ? w : u ? v + 2**-2 : 3,
          c: { i: 0, j: 1, k: 2 },
          d: foo ? { n: 123 } : { n: 321 },
        }`)
        const expected = RecordLiteral({
          a: Binop('1', '+', '1'),
          b: CondExpr(Binop('x', '&&', 'y'),
            'z',
            CondExpr('t',
              'w',
              CondExpr('u',
                Binop('v', '+', Binop('2', '**', Unop('-', '2'))),
                '3'))),
          c: RecordLiteral({ i: '0', j: '1', k: '2' }),
          d: CondExpr('foo',
            RecordLiteral({ n: '123' }),
            RecordLiteral({ n: '321' })),
        })
        assert.deepStrictEqual(observed, expected)
      })
      test('comma-first', () => {
        const observed = parser.Expression.tryParse(
          `{ a: 1
           , b: 2
           , c: 3
           }`)
        const expected = RecordLiteral({ a: '1', b: '2', c: '3' })
        assert.deepStrictEqual(observed, expected)
      })
      test('field names must be valid identifiers', () => {
        assert(!parser.Expression.parse('{ invalid__ident: 1 }').status)
        assert(!parser.Expression.parse('{ _invalid: 1 }').status)
        assert(!parser.Expression.parse('{ $invalid: 1 }').status)
        assert(!parser.Expression.parse('{ "not an identifier at all": 1 }').status)
        assert(!parser.Expression.parse('{ 5: 1 }').status)
        assert(!parser.Expression.parse('{ [1+1]: 1 }').status)
        assert(!parser.Expression.parse('{ method() { Return 10 } }').status)
      })
      test('invalid single comma {,}', () => {
        assert(!parser.Expression.parse('{,}').status)
        assert(!parser.Expression.parse('{ , }').status)
      })
    })
    suite('ArrowFunc', () => {
      test('basic x => x**2', () => {
        const observed = parser.Expression.tryParse('x => x**2')
        const expected = ArrowFunc('x', Binop('x', '**', '2'))
        assert.deepStrictEqual(observed, expected)
      })
      test('looser than CondExpr from the left x => x ? 1 : -1', () => {
        const observed = parser.Expression.tryParse('x => x ? 1 : -1')
        const expected = ArrowFunc('x', CondExpr('x', '1', Unop('-', '1')))
        assert.deepStrictEqual(observed, expected)
      })
      test('tighter than CondExpr from the right x ? y => y+1 : y => y+2', () => {
        const observed = parser.Expression.tryParse('x ? y => y+1 : y => y+2')
        const expected = CondExpr('x',
          ArrowFunc('y', Binop('y', '+', '1')),
          ArrowFunc('y', Binop('y', '+', '2')))
        assert.deepStrictEqual(observed, expected)
      })
      test('nested CondExpr & ArrowFunc', () => {
        // bad style, but it should still parse correctly
        const observed = parser.Expression.tryParse('x ? y => y ? 1 : 2 : y => y ? -1 : -2')
        const expected = CondExpr('x',
          ArrowFunc('y', CondExpr('y', '1', '2')),
          ArrowFunc('y', CondExpr('y', Unop('-', '1'), Unop('-', '2'))))
        assert.deepStrictEqual(observed, expected)
      })
      test('prohibit arrow func nested in exprs except CondExpr', () => {
        assert(!parser.Expression.parse('- x => x').status)
        assert(!parser.Expression.parse('x + y => y').status)
        assert(!parser.Expression.parse('x || y => y').status)
      })
      test('paren params (x) => 1', () => {
        const observed = parser.Expression.tryParse('(x) => 1')
        const expected = ArrowFunc('x', '1')
        assert.deepStrictEqual(observed, expected)
      })
      test('multiple params (x,y,z) => 1', () => {
        const observed = parser.Expression.tryParse('(x,y,z) => 1')
        const expected = ArrowFunc('x y z', '1')
        assert.deepStrictEqual(observed, expected)
      })
      test('no empty params or trailing commas, () => 1, (x, y,) => 1', () => {
        assert(!parser.Expression.parse('() => 1').status)
        assert(!parser.Expression.parse('(x, y,) => 1').status)
      })
      test('one-liner stmt block', () => {
        const indentedParser = parserAtIndent('    ')
        const observed = indentedParser.Expression.tryParse(
          'x => { Let y = x + 1 ; Return 2*y }')
        const expected = ArrowFunc('x', [
          LetStmt('y', Binop('x', '+', '1')),
          ReturnStmt(Binop('2', '*', 'y')),
        ])
        assert.deepStrictEqual(observed, expected)
      })
      test('indented stmt block', () => {
        const indentedParser = parserAtIndent('    ')
        const observed = indentedParser.Expression.tryParse('x => {\n'
          + '        Let y = x + 1\n'
          + '        Return 2*y\n'
          + '    }')
        const expected = ArrowFunc('x', [
          LetStmt('y', Binop('x', '+', '1')),
          ReturnStmt(Binop('2', '*', 'y')),
        ])
        assert.deepStrictEqual(observed, expected)
      })
      test('record literal not allowed as expr w/o parens, same as JS x => { x }', () => {
        assert(!parser.Expression.parse('x => { x }').status)
        const observed = parser.Expression.tryParse('x => ({ x })')
        const expected = ArrowFunc('x', RecordLiteral({ x: 'x' }))
        assert.deepStrictEqual(observed, expected)
      })
    })
  })

  suite('expression operator precedence stack', () => {
    suite('FieldFunc, FieldAccessExpr, CallExpr', () => {
      test('basic MemberExpr record.field', () => {
        const observed = parser.Expression.tryParse('record.field')
        const expected = FieldAccess('record', 'field')
        assert.deepStrictEqual(observed, expected)
      })
      test('basic prefix CallExpr f(x)', () => {
        const observed = parser.Expression.tryParse('f(x)')
        const expected = FnCall('f', 'x')
        assert.deepStrictEqual(observed, expected)
      })
      test('basic infix CallExpr aka method-call value.func(arg)', () => {
        const observed = parser.Expression.tryParse('value.func(arg)')
        const expected = MethodCall('value', 'func', 'arg')
        assert.deepStrictEqual(observed, expected)
      })
      test('call a field access function .field(record)', () => {
        const observed = parser.Expression.tryParse('.field(record)')
        const expected = FnCall('.field', 'record')
        assert.deepStrictEqual(observed, expected)
      })
      test('labeled arguments func(from: 1, to: 100)', () => {
        const observed = parser.Expression.tryParse('func(from: 1, to: 100)')
        const expected = FnCallN('func', { from: '1', to: '100' })
        assert.deepStrictEqual(observed, expected)
      })
      test('labeled method arguments thing.func(from: 1, to: 100)', () => {
        const observed = parser.Expression.tryParse('thing.func(from: 1, to: 100)')
        const expected =
          MethodCallN('thing', 'func', { from: '1', to: '100' })
        assert.deepStrictEqual(observed, expected)
      })
      test('altogether now: mapping a field func over a list', () => {
        const observed = parser.Expression.tryParse(
          `[{ foo: 1, bar: 'whatever' }, { foo: 2, bar: 'lol' }].each(.foo)`)
        const expected = MethodCall(
          ArrayLiteral([RecordLiteral({ foo: '1', bar: "'whatever'" }),
            RecordLiteral({ foo: '2', bar: "'lol'" })]),
          'each',
          '.foo')
        assert.deepStrictEqual(observed, expected)
      })
    })
    suite('UnaryExpr', () => {
      test('basic -2', () => {
        const observed = parser.Expression.tryParse('-2')
        const expected = Unop('-', '2')
        assert.deepStrictEqual(observed, expected)
      })
    })

    suite('ExponentExpr', () => {
      test('basic 2**3', () => {
        const observed = parser.Expression.tryParse('2**3')
        const expected = Binop('2', '**', '3')
        assert.deepStrictEqual(observed, expected)
      })
      test('un-op exponent 2**-3', () => {
        const observed = parser.Expression.tryParse('2**-3')
        const expected = Binop('2', '**', Unop('-', '3'))
        assert.deepStrictEqual(observed, expected)
      })
      test('prohibit ambiguous -2**2', () => {
        // see comment in ExponentExpr source code for more about this ambiguity
        assert(!parser.Expression.parse('-2**2').status)

        const observed1 = parser.Expression.tryParse('(-2)**2')
        const expected1 = Binop(Unop('-', '2'), '**', '2')
        assert.deepStrictEqual(observed1, expected1)

        const observed2 = parser.Expression.tryParse('-(2**2)')
        const expected2 = Unop('-', Binop('2', '**', '2'))
        assert.deepStrictEqual(observed2, expected2)
      })
      test('prohibit ambiguous 2**3**2', () => {
        // see comment in ExponentExpr source code for more about this ambiguity
        assert(!parser.Expression.parse('2**3**2').status)

        const observed1 = parser.Expression.tryParse('(2**3)**2')
        const expected1 = Binop(Binop('2', '**', '3'), '**', '2')
        assert.deepStrictEqual(observed1, expected1)

        const observed2 = parser.Expression.tryParse('2**(3**2)')
        const expected2 = Binop('2', '**', Binop('3', '**', '2'))
        assert.deepStrictEqual(observed2, expected2)
      })
    })

    suite('arithmetic precedence', () => {
      test('MultExpr is left-associative', () => {
        const observed = parser.Expression.tryParse('2/3/4')
        const expected = Binop(Binop('2', '/', '3'), '/', '4')
        assert.deepStrictEqual(observed, expected)
      })
      test('the MDAS (in PEMDAS)', () => {
        const observed = parser.Expression.tryParse('2-3*4+5**-6/7**8')
        const expected = Binop(Binop('2', '-', Binop('3', '*', '4')),
          '+',
          Binop(Binop('5', '**', Unop('-', '6')),
            '/',
            Binop('7', '**', '8')))
        assert.deepStrictEqual(observed, expected)
      })
      test('okay, PEMDAS', () => {
        const observed = parser.Expression.tryParse('2-3*(4+5)**-6/7**8')
        const expected = Binop('2', '-',
          Binop(
            Binop('3', '*', Binop(Binop('4', '+', '5'), '**', Unop('-', '6'))),
            '/',
            Binop('7', '**', '8')))
        assert.deepStrictEqual(observed, expected)
      })
    })

    suite('CompareExpr', () => {
      test('basic a != b', () => {
        const observed = parser.Expression.tryParse('a != b')
        const expected = Binop('a', '!=', 'b')
        assert.deepStrictEqual(observed, expected)
      })
      test('no chaining a != b != c', () => {
        assert(!parser.Expression.parse('a != b != c').status)
      })
      test('"!=" is mutually exclusive with other comparisons', () => {
        assert(!parser.Expression.parse('a != b < c').status)
        assert(!parser.Expression.parse('a < b != c').status)
      })
      test('chaining', () => {
        const observed = parser.Expression.tryParse('a < b == c <= d < e')
        const expected = CompareChain(
          Binop('a', '<',  'b'),
          Binop('b', '==', 'c'),
          Binop('c', '<=', 'd'),
          Binop('d', '<',  'e'))
        assert.deepStrictEqual(observed, expected)
      })
      test('improper chaining a < b > c', () => {
        assert(!parser.Expression.parse('a < b > c').status)
        const observed = parser.Expression.tryParse('a < (b > c)')
        const expected = Binop('a', '<', Binop('b', '>', 'c'))
        assert.deepStrictEqual(observed, expected)
      })
      test('chaining starting with equals a == b < c', () => {
        const observed = parser.Expression.tryParse('a == b < c')
        const expected = CompareChain(
          Binop('a', '==', 'b'), Binop('b', '<', 'c'))
        assert.deepStrictEqual(observed, expected)
      })
      test('chaining starting with equals a == b > c', () => {
        const observed = parser.Expression.tryParse('a == b > c')
        const expected = CompareChain(
          Binop('a', '==', 'b'), Binop('b', '>', 'c'))
        assert.deepStrictEqual(observed, expected)
      })
    })

    suite('logical boolean operators && and ||', () => {
      test('&& conventionally has higher precedence than ||', () => {
        const observed = parser.Expression.tryParse('a && b || c && d')
        const expected = Binop(Binop('a', '&&', 'b'),
          '||', Binop('c', '&&', 'd'))
        assert.deepStrictEqual(observed, expected)
      })
      test('logical and arithmetic precedence', () => {
        const observed = parser.Expression.tryParse('a && b == c > d && e')
        const expected = Binop(
          Binop('a', '&&',
            CompareChain(Binop('b', '==', 'c'), Binop('c', '>', 'd'))),
          '&&', 'e')
        assert.deepStrictEqual(observed, expected)
      })
    })

    suite('CondExpr', () => {
      test('basic a ? b : c', () => {
        const observed = parser.Expression.tryParse('a ? b : c')
        const expected = CondExpr('a', 'b', 'c')
        assert.deepStrictEqual(observed, expected)
      })
      test('precedence with comparisons', () => {
        const observed = parser.Expression.tryParse('a == b && c < d < e ? f + 2 : g**3*4')
        const expected = CondExpr(
          Binop(Binop('a', '==', 'b'),
            '&&', CompareChain(
              Binop('c', '<', 'd'), Binop('d', '<', 'e'))),
          Binop('f', '+', '2'),
          Binop(Binop('g', '**', '3'), '*', '4'))
        assert.deepStrictEqual(observed, expected)
      })
      test('nested conditionals', () => {
        const observed = parser.Expression.tryParse('a ? b ? c : d ? e : f : g ? h : i')
        const expected = CondExpr('a',
          CondExpr('b', 'c', CondExpr('d', 'e', 'f')),
          CondExpr('g', 'h', 'i'))
        assert.deepStrictEqual(observed, expected)
      })
      test('mis-nested conditionals', () => {
        assert(!parser.Expression.parse('a ? b ? c : d ? e : f ? g : h').status)
        assert(!parser.Expression.parse('a ? b ? c : d ? e : f : g ? h : i : j').status)
      })
      test('if-elif-elif-else, postfix', () => {
        const observed = parser.Expression.tryParse(
          `a ? b :
            c ? d :
            e ? f :
            g`)
        const expected = CondExpr('a', 'b',
          CondExpr('c', 'd', CondExpr('e', 'f', 'g')))
        assert.deepStrictEqual(observed, expected)
      })
      test('if-elif-elif-else, prefix', () => {
        const observed = parser.Expression.tryParse(
          `a ? b
          : c ? d
          : e ? f
          : g`)
        const expected = CondExpr('a', 'b', CondExpr(
          'c', 'd', CondExpr('e', 'f', 'g')))
        assert.deepStrictEqual(observed, expected)
      })
    })
  })

  suite('Statements', () => {
    suite('LetStmt', () => {
      test('basic Let a = 1', () => {
        const observed = parser.Statement.tryParse('Let a = 1')
        const expected = LetStmt('a', '1')
        assert.deepStrictEqual(observed, expected)
      })
      test('bigger expression', () => {
        const observed = parser.Statement.tryParse('Let y = 2*x**3*4')
        const expected = LetStmt('y',
          Binop(Binop('2', '*', Binop('x', '**', '3')), '*', '4'))
        assert.deepStrictEqual(observed, expected)
      })
      test('less whitespace Let x=1+2', () => {
        const observed = parser.Statement.tryParse('Let x=1+2')
        const expected = LetStmt('x', Binop('1', '+', '2'))
        assert.deepStrictEqual(observed, expected)
      })
      test('missing whitespace Letx = 1', () => {
        assert(!parser.Statement.parse('Letx = 1').status)
      })
    })

    suite('AfterGotStmt', () => {
      test('basic ~ After got x, y, z ~', () => {
        const observed = parser.Statement.tryParse('~ After got x, y, z ~')
        const expected = AfterGotStmt('x y z')
        assert.deepStrictEqual(observed, expected)
      })
      test('basic no tildes', () => {
        const observed = parser.Statement.tryParse('After got x, y, z')
        const expected = AfterGotStmt('x y z')
        assert.deepStrictEqual(observed, expected)
      })
      test('basic one var', () => {
        const observed = parser.Statement.tryParse('~ After got x ~')
        const expected = AfterGotStmt('x')
        assert.deepStrictEqual(observed, expected)
      })
      test('at least one var required', () => {
        const result = parser.Statement.parse('~ After got ~')
        assert(!result.status)
        assert.strictEqual((result as Failure).index.offset, 12)
      })
    })

    suite('StatementIndentBlock', () => {
      test('basic indent block', () => {
        const observed = TopLevel.tryParse(
            'When evt:\n'
          + '    Change x to x+1'
        )
        const expected = WhenDecl('evt', null, [
          ChangeStmt('x', Binop('x', '+', '1')),
        ])
        assert.deepStrictEqual(observed, expected)
      })
      test('comments in indent block', () => {
        const observed = TopLevel.tryParse(
            'When evt:\n'
          + '    // increment counter\n'
          + '    Change x to x+1\n'
          + '    // also the other counter\n'
          + '    Change y to y+2'
        )
        const expected = WhenDecl('evt', null, [
          ChangeStmt('x', Binop('x', '+', '1')),
          ChangeStmt('y', Binop('y', '+', '2')),
        ])
        assert.deepStrictEqual(observed, expected)

        const underIndentedObserved = TopLevel.tryParse(
            'When evt:\n'
          + '  // comment\n'
          + '    Change x to x+1\n'
          + '  // comment\n'
          + '    Change y to y+2'
        )
        assert.deepStrictEqual(underIndentedObserved, expected)

        const overIndentedObserved = TopLevel.tryParse(
            'When evt:\n'
          + '      // comment\n'
          + '    Change x to x+1\n'
          + '      // comment\n'
          + '    Change y to y+2'
        )
        assert.deepStrictEqual(overIndentedObserved, expected)
      })
      test('trailing comments allowed in program', () => {
        // trailing comments aren't allowed in the indent block, but we want to
        // ensure they're still allowed in the program
        const observed = ProgramParser.tryParse(
            'Mechanical v0.0.1\n'
          + '\n'
          + 'When evt:\n'
          + '    // increment counter\n'
          + '    Change x to x+1\n'
          + '    // done!\n'
        )
        const expected = [
          WhenDecl('evt', null, [
            ChangeStmt('x', Binop('x', '+', '1')),
          ])
        ]
        assert.deepStrictEqual(observed, expected)
      })
      test('blank lines in indent block', () => {
        const observed = TopLevel.tryParse(
            'When evt:\n'
          + '\n' // not indented
          + '  \n' // indented wrong
          + '\n' // no indented again
          + '    Change x to x+1\n' // actual indent
          + '  \n' // another blank line indented wrong
          + '    Change y to y+2'
        )
        const expected = WhenDecl('evt', null, [
          ChangeStmt('x', Binop('x', '+', '1')),
          ChangeStmt('y', Binop('y', '+', '2')),
        ])
        assert.deepStrictEqual(observed, expected)
      })
    })

    suite('StatementBraceBlock', () => {
      test('nested arrow function statement blocks', () => {
        const indentedParser = parserAtIndent('    ')
        const observed = indentedParser.Expression.tryParse('x => {\n'
          + '        Let f = y => {\n'
          + '            Return y + 1\n'
          + '        }\n'
          + '        Let z = x + 2\n'
          + '        Return { z, f }\n'
          + '    }')
        const expected = ArrowFunc('x', [
          LetStmt('f', ArrowFunc('y', [ReturnStmt(Binop('y', '+', '1'))])),
          LetStmt('z', Binop('x', '+', '2')),
          ReturnStmt(RecordLiteral({ z: 'z', f: 'f' })),
        ])
        assert.deepStrictEqual(observed, expected)
      })
      test('nested arrow functions with comments', () => {
        const indentedParser = parserAtIndent('    ')
        const observed = indentedParser.Expression.tryParse('x => { // comment \n'
          + '        Let f = y => { // comment \n'
          + '            Return y + 1 // comment, with close-brace } \n'
          + '        } // comment \n'
          + '        Let z = x + 2 // + comment \n'
          + '        Return { z, f } // unmatched open-quote in comment: " \n'
          + '    }')
        const expected = ArrowFunc('x', [
          LetStmt('f', ArrowFunc('y', [ReturnStmt(Binop('y', '+', '1'))])),
          LetStmt('z', Binop('x', '+', '2')),
          ReturnStmt(RecordLiteral({ z: 'z', f: 'f' })),
        ])
        assert.deepStrictEqual(observed, expected)
      })
      test('indent errors', () => {
        const indentedParser = parserAtIndent('    ')
        const notIndented = indentedParser.Statement.parse('Return x => {\n'
          + '    Return x + 1\n'
          + '    }')
        assert(!notIndented.status)
        assert.strictEqual((notIndented as Failure).index.offset, 18)

        const closeBraceIndented = indentedParser.Statement.parse('Return x => {\n'
          + '        Return x + 1\n'
          + '        }')
        assert(!closeBraceIndented.status)
        assert.strictEqual((closeBraceIndented as Failure).index.offset, 43)

        const closeBraceUnindented = indentedParser.Statement.parse('Return x => {\n'
          + '        Return x + 1\n'
          + '  }')
        assert(!closeBraceUnindented.status)
        assert.strictEqual((closeBraceUnindented as Failure).index.offset, 37)

        const mismatchedIndent = indentedParser.Statement.parse('Return x => {\n'
          + '        Let y = x + 1\n'
          + '      Return y - 1\n'
          + '    }')
        assert(!mismatchedIndent.status)
        assert.strictEqual((mismatchedIndent as Failure).index.offset, 42)

        const overIndented = indentedParser.Statement.parse('Return x => {\n'
          + '        Let y = x + 1\n'
          + '          Return y - 1\n'
          + '    }')
        assert(!overIndented.status)
        assert.strictEqual((overIndented as Failure).index.offset, 46)
      })
    })
  })

  suite('Top-Level Declarations', () => {
    suite('StateDecl', () => {
      test('basic State x = 1+2', () => {
        const observed = TopLevel.tryParse('State x = 1+2')
        const expected = StateDecl('x', Binop('1', '+', '2'))
        assert.deepStrictEqual(observed, expected)
      })
    })

    suite('WhenDecl', () => {
      test('basic no param', () => {
        const observed = TopLevel.tryParse(
            'When btnClick:\n'
          + '    Change x to x+1'
        )
        const expected = WhenDecl('btnClick', null, [
          ChangeStmt('x', Binop('x', '+', '1')),
        ])
        assert.deepStrictEqual(observed, expected)
      })
      test('basic with param', () => {
        const observed = TopLevel.tryParse(
            'When btnClick with context:\n'
          + '    Change x to context'
        )
        const expected = WhenDecl('btnClick', 'context', [
          ChangeStmt('x', 'context'),
        ])
        assert.deepStrictEqual(observed, expected)
      })
    })
  })
})

suite('ProgramParser', () => {
  test('basic program', () => {
    const observed = ProgramParser.tryParse(
        'Mechanical v0.0.1\n'
      + '\n'
      + 'State counter = 0\n'
      + '\n'
      + 'When btnClick:\n'
      + '    Change counter to counter+1\n'
      + '\n'
    )
    const expected = [
      StateDecl('counter', '0'),
      WhenDecl('btnClick', null, [
        ChangeStmt('counter', Binop('counter', '+', '1')),
      ]),
    ]
    assert.deepStrictEqual(observed, expected)
  })
  test('basic program, less whitespace', () => {
    const observed = ProgramParser.tryParse(
        'Mechanical v0.0.1\n'
      + 'State counter = 0\n'
      + 'When btnClick:\n'
      + '    Change counter to counter+1'
    )
    const expected = [
      StateDecl('counter', '0'),
      WhenDecl('btnClick', null, [
        ChangeStmt('counter', Binop('counter', '+', '1')),
      ]),
    ]
    assert.deepStrictEqual(observed, expected)
  })
  test('basic program with comments', () => {
    const observed = ProgramParser.tryParse(
        'Mechanical v0.0.1\n'
      + '\n'
      + '// count how many times button was clicked\n'
      + 'State counter = 0\n'
      + '\n'
      + 'When btnClick:\n'
      + '    // when button is clicked, increment counter\n'
      + '    Change counter to counter+1\n'
      + '\n'
    )
    const expected = [
      StateDecl('counter', '0'),
      WhenDecl('btnClick', null, [
        ChangeStmt('counter', Binop('counter', '+', '1')),
      ]),
    ]
    assert.deepStrictEqual(observed, expected)
  })
  test('basic program with nonempty blank lines', () => {
    const observed = ProgramParser.tryParse(
        'Mechanical v0.0.1\n'
      + '\n'
      + 'State counter = 0\n'
      + '  \n'
      + 'When btnClick:\n'
      + '              \n'
      + '    Change counter to counter+1\n'
      + '\n'
    )
    const expected = [
      StateDecl('counter', '0'),
      WhenDecl('btnClick', null, [
        ChangeStmt('counter', Binop('counter', '+', '1')),
      ]),
    ]
    assert.deepStrictEqual(observed, expected)
  })
  test('Declarations cannot be indented', () => {
    const result1 = ProgramParser.parse(
        'Mechanical v0.0.1\n'
      + '\n'
      + '  State x = 1\n'
      + '\n'
    )
    assert(!result1.status)
    assert.strictEqual((result1 as Failure).index.offset, 21)

    const result2 = ProgramParser.parse(
        'Mechanical v0.0.1\n'
      + '\n'
      + 'State x = 1\n'
      + '  \n'
      + '  When btnClick:\n'
      + '    Change x to x+1\n'
      + '\n'
    )
    assert(!result2.status)
    assert.strictEqual((result2 as Failure).index.offset, 36)
  })
})

suite('Type System', () => {
  suite.skip('type object generators ("arbitraries")', () => {
    // this test suite just sanity-checks that the "arbitraries" produce
    // well-formed type objects

    test('arb_nontrivial_type()', () => fc.assert(
      fc.property(arb_nontrivial_type(100), T => {
        return isWellFormed(T)
      })
    ))

    test('perturb_type()', () => fc.assert(
      fc.property(
        arb_nontrivial_type(100),
        fc.array(
          fc.record({
            str: fc.hexaString({ minLength: 2 }), // reproducible RNGs
            type: arb_nontrivial_type(5), // randomized type that can be inserted
          }),
          { minLength: 10, maxLength: 10},
        ),
        (T, rngs) => {
          const Ts = rngs.map(({str, type}) => {
            const perturbed = perturb_type(T, str, type)
            return T = isWellFormed(T) && perturbed
          })
          return Ts.every(T => isWellFormed(T))
        }
      )
    ))

    test('arb_similar_types()', () => fc.assert(
      fc.property(arb_similar_types(10, arb_nontrivial_type(100)), Ts => {
        return Ts.every(T => isWellFormed(T))
      })
    ))
  })

  const EnumSum = { species: 'Sum', variants: { 'foo': null, 'bar': null }} as const

  const Err =  { species: 'Error', error: EnumSum } as const
  const bool = { species: 'boolean' } as const
  const num =  { species: 'number'  } as const
  const str =  { species: 'string'  } as const

  const nullable = (T: Exclude<Types.NontrivialType, 'None'>) =>
    ({ ...T, nullable: true as const })
  const errorable = (T: Exclude<Types.NontrivialType, 'None' | Types.Err>) =>
    ({ ...T, error: EnumSum })
  const Arr = (T: Types.NontrivialType) =>
    ({ species: 'Array', ItemType: T } as const)
  const Product = (fields: {[name: string]: Types.NontrivialType}) =>
    ({ species: 'Product', fields } as const)
  const Sum = (variants: {[tag: string]: Types.NontrivialType | null}) =>
    ({ species: 'Sum', variants } as const)
  const FuncUnary = (param: Types.NontrivialType, result: Types.NontrivialType) =>
    ({ species: 'Function', paramCount: 1, param, result } as const)
  const FuncBinary = (param1: Types.NontrivialType, param2: Types.NontrivialType, result: Types.NontrivialType, param2Optional?: true) =>
    ({ species: 'Function', paramCount: 2, param1, param2, result, param2Optional } as const)
  const FuncNary = (firstParam: Types.NontrivialType, params: Array<[string, Types.NontrivialType, true?]>, result: Types.NontrivialType) =>
    ({
      species: 'Function', paramCount: 99, result, firstParam,
      labels: params.map(([label]) => label),
      params: params.reduce((p, [label, T]) => ({...p, [label]: T}), {}),
      optionalCount: params.reduce((n, [_, __, opt]) => n + (+!!opt), 0),
    } as const)

  // allow `false` because union() can return `false`
  const assertTypeEqual = (a: Types.Type | false, b: Types.Type | false) =>
    assert.strictEqual(a && Types.stringify(a), b && Types.stringify(b))

  suite('isSubtype', () => {
    test('basics', () => {
      assert(Types.isSubtype('None', nullable(bool)))
      assert(!Types.isSubtype('None', bool))

      assert(Types.isSubtype(Err, errorable(bool)))
      assert(!Types.isSubtype(Err, nullable(bool)))

      assert(Types.isSubtype(Arr(bool), Arr(bool)))
      assert(Types.isSubtype(Arr(bool), Arr(nullable(bool))))
      assert(Types.isSubtype(Arr(bool), nullable(Arr(bool))))
      assert(!Types.isSubtype(Arr(nullable(bool)), Arr(bool)))

      assert(Types.isSubtype(
        Product({ foo: bool, bar: str           }),
        Product({ foo: bool, bar: nullable(str) })))
      assert(!Types.isSubtype(
        Product({ foo: bool, bar: nullable(str) }),
        Product({ foo: bool, bar: str           })))

      assert(Types.isSubtype(
        Product({ foo: bool, bar: str }),
        Product({ foo: bool           })))
      assert(!Types.isSubtype(
        Product({ foo: bool           }),
        Product({ foo: bool, bar: str })))

      assert(Types.isSubtype(
        Sum({ foo: null, bar: str           }),
        Sum({ foo: null, bar: nullable(str) })))
      assert(!Types.isSubtype(
        Sum({ foo: null, bar: nullable(str) }),
        Sum({ foo: null, bar: str           })))

      assert(Types.isSubtype(
        Sum({ foo: bool           }),
        Sum({ foo: bool, bar: str })))
      assert(!Types.isSubtype(
        Sum({ foo: bool, bar: str }),
        Sum({ foo: bool           })))

      // covariant in return type
      assert(Types.isSubtype(FuncUnary(num, str), FuncUnary(num, nullable(str))))
      assert(!Types.isSubtype(FuncUnary(num, nullable(str)), FuncUnary(num, str)))
      // contravariant in parameter type and optionality
      assert(Types.isSubtype(FuncUnary(nullable(num), str), FuncUnary(num, str)))
      assert(!Types.isSubtype(FuncUnary(num, str), FuncUnary(nullable(num), str)))
      assert(Types.isSubtype(
        FuncBinary(num, nullable(bool), str),
        FuncBinary(num, bool,           str)))
      assert(!Types.isSubtype(
        FuncBinary(num, bool,           str),
        FuncBinary(num, nullable(bool), str)))
      assert(Types.isSubtype(
        FuncBinary(num, bool, str, true),
        FuncBinary(num, bool, str)))
      assert(!Types.isSubtype(
        FuncBinary(num, bool, str),
        FuncBinary(num, bool, str, true)))
      assert(Types.isSubtype(
        FuncNary(num, [['foo', nullable(bool)]], str),
        FuncNary(num, [['foo', bool          ]], str)))
      assert(!Types.isSubtype(
        FuncNary(num, [['foo', bool          ]], str),
        FuncNary(num, [['foo', nullable(bool)]], str)))
      assert(Types.isSubtype(
        FuncNary(num, [['foo', bool, true]], str),
        FuncNary(num, [['foo', bool      ]], str)))
      assert(!Types.isSubtype(
        FuncNary(num, [['foo', bool      ]], str),
        FuncNary(num, [['foo', bool, true]], str)))

      // binary with optional argument can be unary
      assert(Types.isSubtype(FuncBinary(num, bool, str, true), FuncUnary(num, str)))
      //     TODO: should optional arguments ^^^^ be required to be nullable????

      // unary cannot be binary that ignores arguments
      assert(!Types.isSubtype(FuncUnary(num, str), FuncBinary(num, bool, str)))
      assert(!Types.isSubtype(FuncUnary(num, str), FuncBinary(num, bool, str, true)))

      // n-ary with all optional arguments can be unary
      assert(Types.isSubtype(
        FuncNary(num, [['foo', bool, true], ['bar', num, true]], str),
        FuncUnary(num, str)))
      assert(!Types.isSubtype(
        FuncUnary(num, str),
        FuncNary(num, [['foo', bool, true], ['bar', num, true]], str)))

      // order of labeled parameters matters (for now)
      assert(!Types.isSubtype(
        FuncNary(num, [['foo', bool], ['bar', num ]], str),
        FuncNary(num, [['bar', num ], ['foo', bool]], str)))

      // optionals can be dropped from the end
      assert(Types.isSubtype(
        FuncNary(num, [['foo', bool], ['bar', num, true]], str),
        FuncNary(num, [['foo', bool]], str)))
      assert(!Types.isSubtype(
        FuncNary(num, [['foo', bool]], str),
        FuncNary(num, [['foo', bool], ['bar', num, true]], str)))

      // but because order matters, they can't be dropped in the middle
      assert(!Types.isSubtype(
        FuncNary(num, [['foo', bool, true], ['bar', num, true]], str),
        FuncNary(num, [['bar', num]], str)))
    })

    suite('partial ordering axioms', () => {
      test('reflexivity', () => fc.assert(
        fc.property(arb_nontrivial_type(3), A => {
          return Types.isSubtype(A, A)
        })
      ))

      test('antisymmetry', () => fc.assert(fc.property(
        arb_type_pairs(arb_nontrivial_type(100),
          (T, U) => {
            if (Types.isSubtype(T, U) && Types.isSubtype(U, T)) {
              return [T, U] as const
            }
          }),
        pairs => {
          for (const [A, B] of pairs) {
            if (Types.stringify(A) !== Types.stringify(B)) {
              assert.fail(`Two types are subtypes of each other, but not equal:\n  ${Types.stringify(A)}\n  ${Types.stringify(B)}`)
            }
          }
        },
      )))

      test('transitivity', () => fc.assert(fc.property(
        arb_similar_types(30, arb_nontrivial_type(100)), Ts => {
          const strs = Ts.map(Types.stringify)

          const isSubtype: boolean[] = new Array(Ts.length * Ts.length)
          for (let i = 0; i < Ts.length; i += 1) {
            for (let j = 0; j < Ts.length; j += 1) {
              isSubtype[i*Ts.length + j] = Types.isSubtype(Ts[i], Ts[j])
            }
          }

          const triples: Types.NontrivialType[][] = []
          for (let i = 0; i < Ts.length - 2; i += 1) {
            for (let j = i + 1; j < Ts.length - 1; j += 1) {
              if (strs[i] === strs[j]) continue
              let a: number, b: number
              if      (isSubtype[i*Ts.length + j]) [a, b] = [i, j]
              else if (isSubtype[j*Ts.length + i]) [a, b] = [j, i]
              else continue
              for (let k = j + 1; k < Ts.length; k += 1) {
                if (strs[i] === strs[k] || strs[j] === strs[k]) continue
                let c: number
                if      (isSubtype[b*Ts.length + k]) c = k
                else if (isSubtype[k*Ts.length + a]) [a, b, c] = [k, a, b]
                else continue
                triples.push([Ts[a], Ts[b], Ts[c]])
              }
            }
          }
          pre(triples.length > 0)
          for (const [A, B, C] of triples) {
            if (!Types.isSubtype(A, C)) {
              assert.fail(`Not transitive:\n${Types.stringify(A)}\n${Types.stringify(B)}\n${Types.stringify(C)}`)
            }
          }
        }
      )))
    })
  })

  suite('union and intersect', () => {
    test('union basics', () => {
      assertTypeEqual(Types.union(str, 'None'), nullable(str))
      assertTypeEqual(Types.union(Err, 'None'), nullable(Err))
      assertTypeEqual(Types.union(Arr(str), 'None'), nullable(Arr(str)))

      assert.strictEqual(Types.union(str, bool), false)
      assert.strictEqual(Types.union(str, Arr(str)), false)

      const ProdXY = Product({ x: bool, y: num })
      const ProdXZ = Product({ x: bool, z: str })
      const ProdX  = Product({ x: bool })
      assertTypeEqual(Types.union(ProdXY, ProdXZ), ProdX)

      const EnumFoo  = Sum({ foo: null })
      const EnumBar  = Sum({ bar: null })
      assertTypeEqual(Types.union(EnumFoo, EnumBar), EnumSum)

      // can't union incompatible sum types
      const SumFooNum = Sum({ foo: num })
      const SumFooStr = Sum({ foo: str })
      assert.strictEqual(Types.union(SumFooNum, SumFooStr), false)
      assert.strictEqual(Types.union(SumFooNum, EnumFoo  ), false)

      // unioning two function types means using one signature to call
      // either of two functions
      // E.g. Let f = (cond ? foo : bar); f(a, thing: b, another: c)

      // covariant in return type
      assertTypeEqual(
        Types.union(FuncUnary(num, str), FuncUnary(num, nullable(str))),
        FuncUnary(num, nullable(str)))
      // contravariant in parameter type and optionality
      assertTypeEqual(
        Types.union(FuncUnary(num, str), FuncUnary(nullable(num), str)),
        FuncUnary(num, str))
      assertTypeEqual(
        Types.union(
          FuncBinary(num, bool, str),
          FuncBinary(num, bool, str, true)),
        FuncBinary(num, bool, str))
      assertTypeEqual(
        Types.union(
          FuncNary(num, [['foo', bool, true]], str),
          FuncNary(num, [['foo', bool      ]], str)),
        FuncNary(num, [['foo', bool]], str))

      // binary with optional argument can be unioned with unary
      assertTypeEqual(
        Types.union(
          FuncBinary(num, bool, str, true),
          FuncUnary(num, str)),
        FuncUnary(num, str))

      // n-ary with all optional arguments can be unioned with unary
      assertTypeEqual(
        Types.union(
          FuncNary(num, [['foo', bool, true], ['bar', num, true]], str),
          FuncUnary(num, str)),
        FuncUnary(num, str))

      // optionals can be dropped from the end
      assertTypeEqual(
        Types.union(
          FuncNary(num, [['foo', bool], ['bar', num, true]], str),
          FuncNary(num, [['foo', bool]], str)),
        FuncNary(num, [['foo', bool]], str))
      assertTypeEqual(
        Types.union(
          FuncNary(num, [['foo', bool, true], ['bar', num, true]], str),
          FuncNary(num, [['foo', bool]], str)),
        FuncNary(num, [['foo', bool]], str))
      assertTypeEqual(
        Types.union(
          FuncNary(num, [['foo', bool, true], ['bar', num, true]], str),
          FuncNary(num, [['foo', bool, true]], str)),
        FuncNary(num, [['foo', bool, true]], str))
      assertTypeEqual(
        Types.union(
          FuncNary(num, [['foo', bool], ['bar', num, true]], str),
          FuncNary(num, [['foo', bool], ['qux', num, true]], str)),
        FuncNary(num, [['foo', bool]], str))
      assert.strictEqual(
        Types.union(
          FuncNary(num, [['foo', bool], ['bar', num, true]], str),
          FuncNary(num, [['foo', bool], ['qux', num      ]], str)),
        false)
    })

    test('intersect basics', () => {
      assertTypeEqual(Types.intersect(nullable(str), str), str)
      assertTypeEqual(Types.intersect(nullable(str), 'None'), 'None')
      assertTypeEqual(Types.intersect(errorable(str), Err), Err)
      assertTypeEqual(Types.intersect(Arr(nullable(str)), Arr(str)), Arr(str))

      assertTypeEqual(Types.intersect(str, 'None'), 'Nothing')
      assertTypeEqual(Types.intersect(str, bool), 'Nothing')
      assertTypeEqual(Types.intersect(str, Arr(str)), 'Nothing')

      const ProdXY  = Product({ x: bool, y: num })
      const ProdXZ  = Product({ x: bool, z: str })
      const ProdXYZ = Product({ x: bool, y: num, z: str })
      assertTypeEqual(Types.intersect(ProdXY, ProdXZ), ProdXYZ)

      const EnumFoo  = Sum({ foo: null })
      const EnumBar  = Sum({ bar: null })
      assertTypeEqual(Types.intersect(EnumFoo, EnumSum), EnumFoo)
      assertTypeEqual(Types.intersect(EnumFoo, EnumBar), 'Nothing')

      const SumFooNum = Sum({ foo: num })
      const SumFooStr = Sum({ foo: str })
      assertTypeEqual(Types.intersect(SumFooNum, SumFooStr), 'Nothing')
      assertTypeEqual(Types.intersect(SumFooNum, EnumFoo  ), 'Nothing')

      // intersecting two function types means calling one function using
      // either of two signatures, e.g. cond ? foo(a) : foo(b, c)

      // covariant in return type
      assertTypeEqual(
        Types.intersect(FuncUnary(num, str), FuncUnary(num, nullable(str))),
        FuncUnary(num, str))
      // contravariant in parameter type and optionality
      assertTypeEqual(
        Types.intersect(FuncUnary(num, str), FuncUnary(nullable(num), str)),
        FuncUnary(nullable(num), str))
      assertTypeEqual(
        Types.intersect(
          FuncBinary(num, bool, str),
          FuncBinary(num, bool, str, true)),
        FuncBinary(num, bool, str, true))
      assertTypeEqual(
        Types.intersect(
          FuncNary(num, [['foo', bool, true]], str),
          FuncNary(num, [['foo', bool      ]], str)),
        FuncNary(num, [['foo', bool, true]], str))

      // binary with optional argument can be intersected with unary
      assertTypeEqual(
        Types.intersect(
          FuncBinary(num, bool, str, true),
          FuncUnary(num, str)),
        FuncBinary(num, bool, str, true))

      // n-ary with all optional arguments can be intersected with unary
      assertTypeEqual(
        Types.intersect(
          FuncNary(num, [['foo', bool, true], ['bar', num, true]], str),
          FuncUnary(num, str)),
        FuncNary(num, [['foo', bool, true], ['bar', num, true]], str))

      // optionals can be created at the end
      assertTypeEqual(
        Types.intersect(
          FuncNary(num, [['foo', bool], ['bar', num]], str),
          FuncNary(num, [['foo', bool]], str)),
        FuncNary(num, [['foo', bool], ['bar', num, true]], str))
      assertTypeEqual(
        Types.intersect(
          FuncNary(num, [['foo', bool], ['bar', num]], str),
          FuncNary(num, [['foo', bool, true]], str)),
        FuncNary(num, [['foo', bool, true], ['bar', num, true]], str))
      assertTypeEqual(
        Types.intersect(
          FuncNary(num, [['foo', bool], ['bar', num, true]], str),
          FuncNary(num, [['foo', bool], ['qux', num, true]], str)),
        'Nothing')
    })

    test('returns well-formed type objects', () => {
      function assertWellFormedUnionAndIntersect(A: Types.NontrivialType, B: Types.NontrivialType) {
        const U = Types.union(A, B)
        if (!U) {
          if (U !== false) {
            assert.fail('The only valid falsy result of union(A, B)'
              + ' is `false`, but got: ' + U + '\n'
              + 'A = ' + Types.stringify(A) + '\n'
              + 'B = ' + Types.stringify(B) + '\n'
              + 'A (JSON): ' + JSON.stringify(A, null, 2) + '\n'
              + 'B (JSON): ' + JSON.stringify(B, null, 2))
          }
        }
        else {
          assert(isWellFormed(U, () => ({
            A: Types.stringify(A),
            B: Types.stringify(B),
            A_json: A,
            B_json: B,
          })))
        }

        const I = Types.intersect(A, B)
        assert(isWellFormed(I, () => ({
          A: Types.stringify(A),
          B: Types.stringify(B),
          A_json: A,
          B_json: B,
        })))
      }
      fc.assert(fc.property(
        arb_nontrivial_type(100), arb_nontrivial_type(100), (A, B) => {
          assertWellFormedUnionAndIntersect(A, B)
        }
      ))
      fc.assert(fc.property(
        arb_similar_types(30, arb_nontrivial_type(100)), Ts => {
          for (let i = 0; i < Ts.length - 1; i += 1) {
            for (let j = i + 1; j < Ts.length; j += 1) {
              assertWellFormedUnionAndIntersect(Ts[i], Ts[j])
            }
          }
        }
      ))
    })

    test('union/intersect with subtype/supertype is identity (if A ⊆ B, then A ∪ B = B and A ∩ B = A)', () =>
      fc.assert(fc.property(
        arb_type_pairs(arb_nontrivial_type(100),
          (T, U) => {
            if (Types.isSubtype(T, U)) return [T, U] as const
            if (Types.isSubtype(U, T)) return [U, T] as const
          }),
        pairs => {
          for (const [A, B] of pairs) {
            const U = Types.union(B, A)
            if (!U || Types.stringify(U) !== Types.stringify(B)) {
              assert.fail('A ⊆ B, but A ∪ B ≠ B\n'
                + `A     = ${Types.stringify(A)}\n`
                + `B     = ${Types.stringify(B)}\n`
                + `A ∪ B = ${U && Types.stringify(U)}`)
            }

            const I = Types.intersect(A, B)
            if (Types.stringify(I) !== Types.stringify(A)) {
              assert.fail('A ⊆ B, but A ∩ B ≠ A\n'
                + `A     = ${Types.stringify(A)}\n`
                + `B     = ${Types.stringify(B)}\n`
                + `A ∩ B = ${I && Types.stringify(I)}`)
            }
          }
        },
      ))
    )

    test('union is least upper bound (supremum)', () => fc.assert(
      fc.property(arb_similar_types(30, arb_nontrivial_type(100)), Ts => {
        const unionsAndAbove: {A: Types.NontrivialType, B: Types.NontrivialType, U: Types.NontrivialType, above: Types.NontrivialType[]}[] = []
        for (let i = 0; i < Ts.length - 1; i += 1) {
          for (let j = i + 1; j < Ts.length; j += 1) {
            const [A, B] = [Ts[i], Ts[j]]
            const U = Types.union(A, B)
            if (!U) continue
            const above = Ts.filter(T =>
              Types.isSubtype(A, T) && Types.isSubtype(B, T))
            if (above.length > 1) unionsAndAbove.push({ A, B, U, above })
          }
        }
        pre(unionsAndAbove.length > 0)
        for (const { A, B, U, above } of unionsAndAbove) {
          for (const T of above) {
            if (!Types.isSubtype(U, T)) {
              assert.fail('T is a supertype of both A and B, but not of A ∪ B:\n'
                + `A     = ${Types.stringify(A)}\n`
                + `B     = ${Types.stringify(B)}\n`
                + `A ∪ B = ${Types.stringify(U)}\n`
                + `T     = ${Types.stringify(T)}`)
            }
          }
        }
      })
    ))

    test('intersection is greatest lower bound (infimum)', () => fc.assert(
      fc.property(arb_similar_types(30, arb_nontrivial_type(100)), Ts => {
        const intersectsAndBelow: {A: Types.NontrivialType, B: Types.NontrivialType, I: Types.NontrivialType, below: Types.NontrivialType[]}[] = []
        for (let i = 0; i < Ts.length - 1; i += 1) {
          for (let j = i + 1; j < Ts.length; j += 1) {
            const [A, B] = [Ts[i], Ts[j]]
            const I = Types.intersect(A, B)
            if (I === 'Nothing') continue
            const below = Ts.filter(T =>
              Types.isSubtype(T, A) && Types.isSubtype(T, B))
            if (below.length > 1) intersectsAndBelow.push({ A, B, I, below })
          }
        }
        pre(intersectsAndBelow.length > 0)
        for (const { A, B, I, below } of intersectsAndBelow) {
          for (const T of below) {
            if (!Types.isSubtype(T, I)) {
              assert.fail('T is a subtype of both A and B, but not of A ∩ B:\n'
                + `A     = ${Types.stringify(A)}\n`
                + `B     = ${Types.stringify(B)}\n`
                + `A ∩ B = ${Types.stringify(I)}\n`
                + `T     = ${Types.stringify(T)}`)
            }
          }
        }
      })
    ))

    suite('partial lattice join and meet axioms', () => {
      // only a partial lattice because not all pairs of types
      // have a union, e.g. there's no union of string and number.
      // All pairs of types have an intersect, though, so our
      // type system is valid meet semi-lattice (lower semi-lattice)
      // https://en.wikipedia.org/wiki/Join_and_meet
      // https://en.wikipedia.org/wiki/Lattice_(order)#General_lattice

      test('idempotence', () => fc.assert(
        fc.property(arb_nontrivial_type(100), T => {
          assertTypeEqual(T, Types.union(T, T))
          assertTypeEqual(T, Types.intersect(T, T))
        })
      ))

      test('commutativity', () => fc.assert(fc.property(
        arb_type_pairs(arb_nontrivial_type(100), (A, B) => [A, B] as const),
        pairs => {
          for (const [A, B] of pairs) {
            assertTypeEqual(Types.union(A, B), Types.union(B, A))
            assertTypeEqual(Types.intersect(A, B), Types.intersect(B, A))
          }
        },
      )))

      test('associativity', () => fc.assert(fc.property(
        arb_similar_types(25, arb_nontrivial_type(100)), Ts => {
          const width = Ts.length - 1
          const unions: (Types.NontrivialType | false)[]         = new Array(width * Ts.length)
          const intersects: (Types.NontrivialType | 'Nothing')[] = new Array(width * Ts.length)
          for (let i = 0; i < width; i += 1) {
            for (let j = i + 1; j < Ts.length; j += 1) {
              unions[i*width + j] = Types.union(Ts[i], Ts[j])
              intersects[i*width + j] = Types.intersect(Ts[i], Ts[j])
            }
          }
          for (let i = 0; i < Ts.length - 2; i += 1) {
            for (let j = i + 1; j < Ts.length - 1; j += 1) {
              for (let k = j + 1; k < Ts.length; k += 1) {
                const A = Ts[i]
                const C = Ts[k]
                const AuB = unions[i*width + j]
                const BuC = unions[j*width + k]
                if (AuB && BuC) {
                  const AuB_uC = Types.union(AuB, C)
                  const Au_BuC = Types.union(A, BuC)
                  const str_AuB_uC = AuB_uC && Types.stringify(AuB_uC)
                  const str_Au_BuC = Au_BuC && Types.stringify(Au_BuC)
                  if (str_AuB_uC !== str_Au_BuC) {
                    assert.fail('Non-associativity: (A ∪ B) ∪ C ≠ A ∪ (B ∪ C)\n'
                      + `          A = ${Types.stringify(A)}\n`
                      + `          B = ${Types.stringify(Ts[j])}\n`
                      + `          C = ${Types.stringify(C)}\n`
                      + `      A ∪ B = ${Types.stringify(AuB)}\n`
                      + `      B ∪ C = ${Types.stringify(BuC)}\n`
                      + `(A ∪ B) ∪ C = ${str_AuB_uC}\n`
                      + `A ∪ (B ∪ C) = ${str_Au_BuC}\n`
                      + '\n'
                      + `A (JSON): ${JSON.stringify(A,     null, 2)}\n`
                      + `B (JSON): ${JSON.stringify(Ts[j], null, 2)}\n`
                      + `C (JSON): ${JSON.stringify(C,     null, 2)}\n`)
                  }
                }
                const AnB = intersects[i*width + j]
                const BnC = intersects[j*width + k]
                const AnB_nC = Types.intersect(AnB, C)
                const An_BnC = Types.intersect(A, BnC)
                const str_AnB_nC = Types.stringify(AnB_nC)
                const str_An_BnC = Types.stringify(An_BnC)
                if (str_AnB_nC !== str_An_BnC) {
                  assert.fail('Non-associativity: (A ∩ B) ∩ C ≠ A ∩ (B ∩ C)\n'
                    + `          A = ${Types.stringify(A)}\n`
                    + `          B = ${Types.stringify(Ts[j])}\n`
                    + `          C = ${Types.stringify(C)}\n`
                    + `      A ∩ B = ${Types.stringify(AnB)}\n`
                    + `      B ∩ C = ${Types.stringify(BnC)}\n`
                    + `(A ∩ B) ∩ C = ${str_AnB_nC}\n`
                    + `A ∩ (B ∩ C) = ${str_An_BnC}\n`
                    + '\n'
                    + `A (JSON): ${JSON.stringify(A,     null, 2)}\n`
                    + `B (JSON): ${JSON.stringify(Ts[j], null, 2)}\n`
                    + `C (JSON): ${JSON.stringify(C,     null, 2)}\n`)
                }
              }
            }
          }
        }
      )))

      test('absorption', () => fc.assert(fc.property(
        arb_type_pairs(arb_nontrivial_type(100), (A, B) => [A, B] as const),
        pairs => {
          for (const [A, B] of pairs) {
            const U = Types.union(A, B)
            const I = Types.intersect(A, B)
            if (U) {
              assertTypeEqual(Types.intersect(A, U), A)
              assertTypeEqual(Types.intersect(B, U), B)
            }
            assertTypeEqual(Types.union(A, I), A)
            assertTypeEqual(Types.union(B, I), B)
          }
        }
      )))
    })
  })
})

suite('codegen', () => {
  const ctx = {
    indent: '',
    scope: { foo: 'foo_' },
  } as const
  suite('primary exprs', () => {
    test('scalars (numerals, boolean and string literals)', () => {
      assert.strictEqual(codegenExpr(ctx, Term('123')),    '123')
      assert.strictEqual(codegenExpr(ctx, Term('"asdf"')), '"asdf"')
    })
    test('FieldFunc', () => {
      const observed = codegenExpr(ctx, Term('.field_name'))
      const expected = '(record => record.field_name)'
      assert.strictEqual(observed, expected)
    })
    test('Variable', () => {
      const observed = codegenExpr(ctx, Term('foo'))
      const expected = 'foo_'
      assert.strictEqual(observed, expected)

      assert.throws(() => {
        codegenExpr(ctx, Term('asdf'))
      })
    })
    test('ArrayLiteral', () => {
      const observed1 = codegenExpr(ctx,
        ArrayLiteral([ '1', 'foo', '3' ]))
      const expected1 = '[1, foo_, 3]'
      assert.strictEqual(observed1, expected1)

      const observed2 = codegenExpr(ctx, ArrayLiteral([]))
      const expected2 = '[]'
      assert.strictEqual(observed2, expected2)
    })
    test('RecordLiteral', () => {
      const observed0 = codegenExpr(ctx, RecordLiteral({}))
      const expected0 = '{}'
      assert.strictEqual(observed0, expected0)

      const observed1 = codegenExpr(ctx, RecordLiteral({ a: '1' }))
      const expected1 = '{ a: 1 }'
      assert.strictEqual(observed1, expected1)

      const observed2 = codegenExpr(ctx,
        RecordLiteral({ a: '1', b: 'foo', c: '3' }))
      const expected2 = '{\n'
        + '  a: 1,\n'
        + '  b: foo_,\n'
        + '  c: 3\n'
        + '}'
      assert.strictEqual(observed2, expected2)

      const observed3 = codegenExpr({ ...ctx, indent: '    ' },
        RecordLiteral({
          a: RecordLiteral({}),
          b: RecordLiteral({ c: '1', }),
          d: RecordLiteral({ e: '2', f: '3' }),
        }))
      const expected3 = '{\n'
        + '      a: {},\n'
        + '      b: { c: 1 },\n'
        + '      d: {\n'
        + '        e: 2,\n'
        + '        f: 3\n'
        + '      }\n'
        + '    }'
      assert.strictEqual(observed3, expected3)
    })
  })
  suite('FieldAccessExpr', () => {
    test('basic', () => {
      const observed = codegenExpr(ctx, FieldAccess('foo', 'field'))
      const expected = 'foo_.field'
      assert.strictEqual(observed, expected)
    })
    test('precedence: no parens needed x.y.foo().z', () => {
      const observed = codegenExpr(
        { ...ctx, scope: { ...ctx.scope, x: 'x' } },
        FieldAccess(MethodCall(FieldAccess('x', 'y'), 'foo'), 'z'))
      const expected = 'foo_(x.y).z'
      assert.strictEqual(observed, expected)
    })
    test('precedence: need parens (x + y).z', () => {
      const observed = codegenExpr(
        { ...ctx, scope: { x: 'x', y: 'y' } },
        FieldAccess(Binop('x', '+', 'y'), 'z'))
      const expected = '(x + y).z'
      assert.strictEqual(observed, expected)
    })
  })
  suite('CallExprs', () => {
    test('basic', () => {
      const observed = codegenExpr(ctx, FnCall('foo', '1'))
      const expected = 'foo_(1)'
      assert.deepStrictEqual(observed, expected)
    })
    test('method call', () => {
      const observed = codegenExpr(ctx, MethodCall('1', 'foo', '2'))
      const expected = 'foo_(1, 2)'
      assert.deepStrictEqual(observed, expected)
    })
    test('precedence: need parens (x ? foo : bar)(1)', () => {
      const observed = codegenExpr(
        { ...ctx, scope: { x: 'y', foo: 'foo', bar: 'bar' } },
        FnCall(CondExpr('x', 'foo', 'bar'), '1'))
      const expected = '(y ? foo : bar)(1)'
      assert.deepStrictEqual(observed, expected)
    })
    test('precedence: need parens (x + y).foo(1)', () => {
      const observed = codegenExpr(
        { ...ctx, scope: { ...ctx.scope, x: 'x', y: 'y' } },
        MethodCall(Binop('x', '+', 'y'), 'foo', '1'))
      const expected = 'foo_(x + y, 1)'
      assert.deepStrictEqual(observed, expected)
    })
  })
  suite('unops and binops', () => {
    test('PEMDAS', () => {
      const observed = codegenExpr(ctx,
        Binop('2', '-',
          Binop(
            Binop('3', '*', Binop(Binop('4', '+', '5'), '**', Unop('-', '6'))),
            '/',
            Binop('7', '**', '8'))))
      const expected = '2 - 3*(4 + 5)**-6/7**8'
      assert.strictEqual(observed, expected)
    })
    test('full associativity', () => {
      const observed = codegenExpr(ctx,
        Binop(
          Binop('1', '+', '2'),
          '+',
          Binop(
            Binop(Binop('3', '*', '4'), '*', Binop('5', '*', '6')),
            '+',
            Binop('7', '+', '8'))))
      const expected = '1 + 2 + 3*4*5*6 + 7 + 8'
      assert.strictEqual(observed, expected)
    })
    test('left-associative ops', () => {
      const observed = codegenExpr(ctx,
        Binop(
          Binop(
            Binop(
              Binop('1', '/', '2'),
              '/',
              Binop('3', '/', '4')),
            '-',
            '5'),
          '-',
          Binop(
            '6',
            '-',
            Binop(
              Binop('7', '%', '8'),
              '%',
              Binop('9', '%', '10')))))
      const expected = '1/2/(3/4) - 5 - (6 - 7 % 8 % (9 % 10))'
      assert.strictEqual(observed, expected)
    })
    test('non-associative ops', () => {
      const observed = codegenExpr(ctx,
        Binop(
          Binop('1', '<', '2'),
          '==',
          Binop('3', '<=', '4')))
      const expected = '(1 < 2) === (3 <= 4)'
      assert.strictEqual(observed, expected)
    })
    test('exclusive precedence', () => {
      const scope = { a: 'a', b: 'b', c: 'c', d: 'd', e: 'e', f: 'f',
        g: 'g', h: 'h', i: 'i', j: 'j' }
      const observed = codegenExpr({ ...ctx, scope },
        Binop(
          Binop(
            Binop(
              Binop(Binop('a', '||', 'b'), '&&', 'c'),
              '&&',
              Binop('d', '&&', Binop('e', '||', 'f'))),
            '||',
            'g'),
          '||',
          Binop('h', '||', Binop('i', '&&', 'j'))))
      const expected = '((a || b) && c && d && (e || f)) || g || h || (i && j)'
      assert.strictEqual(observed, expected)
    })
    test('CompareChainExpr', () => {
      const scope = { a: 'a', b: 'b', c: 'c', d: 'd', e: 'e', f: 'f',
        g: 'g', h: 'h', i: 'i', j: 'j' }
      const observed = codegenExpr({ ...ctx, scope },
        Binop(
          CompareChain(
            Binop('1', '<', '2'),
            Binop('2', '==', '3'),
            Binop('3', '<=', '4')),
          '&&',
          CompareChain(
            Binop('9', '>', '8'),
            Binop('8', '>=', '7'))))
      const expected = '1 < 2 && 2 === 3 && 3 <= 4 && 9 > 8 && 8 >= 7'
      assert.strictEqual(observed, expected)
    })
    test('spacing', () => {
      const observed = codegenExpr(ctx,
        Binop(
          Binop(
            Binop(
              '1', '+', Binop(Binop(Binop('2', '*', '3'), '/', '4'), '%', '5')),
            '-',
            '6'),
          '==',
          '7'))
      const expected = '1 + 2*3/4 % 5 - 6 === 7'
      assert.strictEqual(observed, expected)
    })
  })
  suite('CondExpr', () => {
    test('basic', () => {
      const observed = codegenExpr(ctx,
        CondExpr(Binop('1', '<', '2'), '3', '4'))
      const expected = '1 < 2 ? 3 : 4'
      assert.strictEqual(observed, expected)
    })
    test('right-associativity (sort of)', () => {
      const scope = { a: 'a', b: 'b', c: 'c', d: 'd', e: 'e', f: 'f',
        g: 'g', h: 'h', i: 'i', j: 'j' }
      const observed1 = codegenExpr({ ...ctx, scope },
        CondExpr('a', 'b', CondExpr('c', 'd', 'e')))
      const expected1 = 'a ? b : c ? d : e'
      assert.strictEqual(observed1, expected1)

      const observed2 = codegenExpr({ ...ctx, scope },
        CondExpr('a', CondExpr('b', 'c', 'd'), 'e'))
      const expected2 = 'a ? b ? c : d : e'
      assert.strictEqual(observed2, expected2)

      const observed3 = codegenExpr({ ...ctx, scope },
        CondExpr(CondExpr('a', 'b', 'c'), 'd', 'e'))
      const expected3 = '(a ? b : c) ? d : e'
      assert.strictEqual(observed3, expected3)
    })
    test('more complex precedence', () => {
      const observed = codegenExpr(ctx, Binop('1', '+', CondExpr(
        Binop(Binop('2', '+', '3'), '>', '4'),
        '5',
        Binop(Binop('6', '*', '7'), '-', '8'))))
      const expected = '1 + (2 + 3 > 4 ? 5 : 6*7 - 8)'
      assert.strictEqual(observed, expected)
    })
  })
  suite('ArrowFunc', () => {
    test('basic', () => {
      const observed1 = codegenExpr(ctx, ArrowFunc('x', Binop('x', '**', '2')))
      const expected1 = 'x => x**2'
      assert.strictEqual(observed1, expected1)

      const observed2 = codegenExpr(ctx,
        ArrowFunc('x y', Binop('x', '+', 'y')))
      const expected2 = '(x, y) => x + y'
      assert.strictEqual(observed2, expected2)
    })
    test('precedence and parenthesization', () => {
      const ctx = { indent: '', scope: { x: 'x' } }
      const observed1 = codegenExpr(ctx,
        ArrowFunc('x', CondExpr('x', '1', Unop('-', '1'))))
      const expected1 = 'x => x ? 1 : -1'
      assert.strictEqual(observed1, expected1)

      const observed2 = codegenExpr(ctx, CondExpr('x',
        ArrowFunc('y', Binop('y', '+', '1')),
        ArrowFunc('y', Binop('y', '+', '2'))))
      const expected2 = 'x ? (y => y + 1) : (y => y + 2)'
      assert.strictEqual(observed2, expected2)

      const observed3 = codegenExpr(ctx, CondExpr('x',
        ArrowFunc('y', CondExpr('y', '1', '2')),
        ArrowFunc('y', CondExpr('y', Unop('-', '1'), Unop('-', '2')))))
      const expected3 = 'x ? (y => y ? 1 : 2) : (y => y ? -1 : -2)'
      assert.strictEqual(observed3, expected3)
    })
    test('JS reserved words', () => {
      const observed = codegenExpr(ctx, ArrowFunc('try new function',
        Binop(Binop('try', '+', 'new'), '+', 'function')))
      const expected = '(try_, new_, function_) => try_ + new_ + function_'
      assert.strictEqual(observed, expected)
    })
  })
  suite('simple statements', () => {
    test('ReturnStmt', () => {
      const observed = codegenStmt(ctx,
        ReturnStmt(Binop(Binop('2', '*', 'foo'), '+', '1')))
      const expected = { code: 'return 2*foo_ + 1;\n', newVar: undefined }
      assert.deepStrictEqual(observed, expected)
    })
    test('EmitStmt', () => {
      const observed = codegenStmt(ctx,
        EmitStmt(Binop(Binop('2', '*', 'foo'), '+', '1')))
      const expected = { code: '$emit(2*foo_ + 1);\n', newVar: undefined }
      assert.deepStrictEqual(observed, expected)
    })
    test('DoStmt', () => {
      const observed1 = codegenStmt(ctx, DoStmt('foo'))
      const expected1 = { code: 'foo_();\n', newVar: undefined }
      assert.deepStrictEqual(observed1, expected1)

      const observed2 = codegenStmt(ctx, DoStmt(FnCall('foo', '1')))
      const expected2 = { code: 'foo_(1)();\n', newVar: undefined }
      assert.deepStrictEqual(observed2, expected2)
    })
  })
  suite('LetStmt', () => {
    test('basic arrow function', () => {
      const observed = codegenExpr(ctx, ArrowFunc('x', [
        LetStmt('y', Binop('x', '+', '1')),
        ReturnStmt('y'),
      ]))
      const expected = 'x => {\n'
        + '  const y = x + 1;\n'
        + '  return y;\n'
        + '}'
      assert.strictEqual(observed, expected)
    })
    test('JS reserved words', () => {
      const observed = codegenExpr(ctx, ArrowFunc('for', [
        LetStmt('return', Binop('for', '+', '1')),
        ReturnStmt('return'),
      ]))
      const expected = 'for_ => {\n'
        + '  const return_ = for_ + 1;\n'
        + '  return return_;\n'
        + '}'
      assert.strictEqual(observed, expected)
    })
  })
})

suite('Compile & Run', () => {
  test('basic program', async () => {
    const observed = compile(
        'Mechanical v0.0.1\n'
      + '\n'
      + 'Do console_log("hello, world")\n'
    )
    const expected =
        '// runtime:\n'
      + 'const console_log = (...args) => () => console.log(...args);\n'
      + '\n'
      + '// State declarations: (TODO)\n'
      + '\n'
      + '// initializing statements:\n'
      + 'console_log("hello, world")();\n'
      + '\n'
      + '// When declarations: (TODO)\n'
    assert.strictEqual(observed, expected)

    const observedRun = await new Promise((resolve, reject) => {
      const run = exec('node', (err, stdout, stderr) => {
        if (err) reject(err)
        else if (stderr) reject('stderr: ' + stderr)
        else resolve(stdout)
      })
      Readable.from(observed).pipe(run.stdin!)
    })
    const expectedRun = 'hello, world\n'
    assert.strictEqual(observedRun, expectedRun)
  })
})
