import 'mocha'
import assert from 'assert'
import { Failure } from 'parsimmon'

import { parserAtIndent, parser } from './mechc'

suite('Parser', () => {
  test('StateDecl', () => {
    const observed = parser.StateDecl.tryParse('State x = ')
    const expected = { var_name: 'x' }
    assert.deepStrictEqual(observed, expected)
  })

  suite('primary exprs', () => {
    suite('identifiers', () => {
      test('basic this_is_valid', () => {
        const observed = parser.PrimaryExpr.tryParse('this_is_valid')
        const expected = 'this_is_valid'
        assert.deepStrictEqual(observed, expected)
      })
      test('invalid _foo, foo__bar, foo_, $foo', () => {
        assert(!parser.PrimaryExpr.parse('_foo').status)
        assert(!parser.PrimaryExpr.parse('foo__bar').status)
        assert(!parser.PrimaryExpr.parse('foo_').status)
        assert(!parser.PrimaryExpr.parse('$foo').status)
      })
    })
    suite('string literals', () => {
      test('basic "asdf"', () => {
        const observed = parser.PrimaryExpr.tryParse('"asdf"')
        const expected = '"asdf"'
        assert.deepStrictEqual(observed, expected)
      })
      test("basic 'asdf'", () => {
        const observed = parser.PrimaryExpr.tryParse("'asdf'")
        const expected = "'asdf'"
        assert.deepStrictEqual(observed, expected)
      })
      test('basic ""', () => {
        const observed = parser.PrimaryExpr.tryParse('""')
        const expected = '""'
        assert.deepStrictEqual(observed, expected)
      })
      test("basic ''", () => {
        const observed = parser.PrimaryExpr.tryParse("''")
        const expected = "''"
        assert.deepStrictEqual(observed, expected)
      })
      test('escaping double-quotes', () => {
        const observed = parser.PrimaryExpr.tryParse('"you could call it \\"weird\\", I guess"')
        const expected = '"you could call it \\"weird\\", I guess"'
        assert.deepStrictEqual(observed, expected)
      })
      test('escaping single-quotes', () => {
        const observed = parser.PrimaryExpr.tryParse("'you could call it \\'weird\\', I guess'")
        const expected = "'you could call it \\'weird\\', I guess'"
        assert.deepStrictEqual(observed, expected)
      })
      test('multiline double-quotes', () => {
        const observed = parser.PrimaryExpr.tryParse('"first line\nsecond line"')
        const expected = '"first line\nsecond line"'
        assert.deepStrictEqual(observed, expected)
      })
      test('multiline single-quotes', () => {
        const observed = parser.PrimaryExpr.tryParse("'first line\nsecond line'")
        const expected = "'first line\nsecond line'"
        assert.deepStrictEqual(observed, expected)
      })
      test('indented multiline double-quotes', () => {
        const indentedParser = parserAtIndent('  ')
        const observed = indentedParser.PrimaryExpr.tryParse('"first\n  second\n    third\n  fourth"')
        const expected = '"first\nsecond\n  third\nfourth"'
        assert.deepStrictEqual(observed, expected)
      })
      test('indented multiline single-quotes', () => {
        const indentedParser = parserAtIndent('  ')
        const observed = indentedParser.PrimaryExpr.tryParse("'first\n  second\n    third\n  fourth'")
        const expected = "'first\nsecond\n  third\nfourth'"
        assert.deepStrictEqual(observed, expected)
      })
      test('multiline string requires indent', () => {
        const indentedParser = parserAtIndent('    ')
        const result = indentedParser.PrimaryExpr.parse('"first\n   second"')
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
        const expected = {
          type: 'ArrowFunc',
          params: ['x'],
          body: [
            {
              type: 'LetStmt',
              varName: 'y',
              expr: '"first\n  second\nthird"',
            },
            {
              type: 'ReturnStmt',
              expr: 'y',
            },
          ],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('mismatched quotes', () => {
        assert(!parser.PrimaryExpr.parse('"text').status)
        assert(!parser.PrimaryExpr.parse('"text \\"something" else"').status)
        assert(!parser.PrimaryExpr.parse("'text").status)
        assert(!parser.PrimaryExpr.parse("'text \\'something' else'").status)
      })
    })
    suite('array literals', () => {
      test('basic [1,2,3]', () => {
        const observed = parser.PrimaryExpr.tryParse('[ 1, 2, 3 ]')
        const expected = {
          type: 'ArrayLiteral',
          exprs: [ '1', '2', '3' ],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('basic empty []', () => {
        const observed = parser.PrimaryExpr.tryParse('[]')
        const expected = {
          type: 'ArrayLiteral',
          exprs: [],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('trailing comma [1,2,]', () => {
        const observed = parser.PrimaryExpr.tryParse('[ 1, 2, ]')
        const expected = {
          type: 'ArrayLiteral',
          exprs: [ '1', '2' ],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('newlines [1,2,]', () => {
        const observed = parser.PrimaryExpr.tryParse(`[
          1,
          2,
        ]`)
        const expected = {
          type: 'ArrayLiteral',
          exprs: [ '1', '2' ],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('comma-first', () => {
        const observed = parser.PrimaryExpr.tryParse(
          `[ 1
           , 2
           , 3
           ]`)
        const expected = {
          type: 'ArrayLiteral',
          exprs: [ '1', '2', '3' ],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('invalid (holes) [1,,2]', () => {
        assert(!parser.PrimaryExpr.parse('[ 1, , 2 ]').status)
        assert(!parser.PrimaryExpr.parse('[ 1, 2,, ]').status)
      })
      test('invalid single comma [,]', () => {
        assert(!parser.PrimaryExpr.parse('[,]').status)
        assert(!parser.PrimaryExpr.parse('[ , ]').status)
      })
      test('invalid (missing comma) [1 2]', () => {
        assert(!parser.PrimaryExpr.parse('[1 2]').status)
        assert(!parser.PrimaryExpr.parse('[[1] 2]').status)
        assert(!parser.PrimaryExpr.parse('[1 [2]]').status)
      })
      test('invalid mismatched brackets', () => {
        assert(!parser.PrimaryExpr.parse('[1, 2').status)
        assert(!parser.PrimaryExpr.parse('1, 2]').status)
        assert(!parser.PrimaryExpr.parse('[[1, 2]').status)
        assert(!parser.PrimaryExpr.parse('[[1], 2').status)
      })
    })
    suite('record literals', () => {
      test('basic {a: 1, b:2}', () => {
        const observed = parser.PrimaryExpr.tryParse('{a: 1, b: 2}')
        const expected = {
          type: 'RecordLiteral',
          pairs: [
            { key: 'a', val: '1' },
            { key: 'b', val: '2' },
          ],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('trailing comma {a: 1, b:2,}', () => {
        const observed = parser.PrimaryExpr.tryParse('{a: 1, b: 2,}')
        const expected = {
          type: 'RecordLiteral',
          pairs: [
            { key: 'a', val: '1' },
            { key: 'b', val: '2' },
          ],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('empty record {}', () => {
        const observed = parser.PrimaryExpr.tryParse('{}')
        const expected = {
          type: 'RecordLiteral',
          pairs: [],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('record field name punning {a}', () => {
        const observed = parser.PrimaryExpr.tryParse('{a}')
        const expected = {
          type: 'RecordLiteral',
          pairs: [{ key: 'a', val: 'a' }],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('mixed obj { a: 1, b, c, }', () => {
        const observed = parser.PrimaryExpr.tryParse('{ a: 1, b, c, }')
        const expected = {
          type: 'RecordLiteral',
          pairs: [
            { key: 'a', val: '1' },
            { key: 'b', val: 'b' },
            { key: 'c', val: 'c' },
          ],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('invalid without comma {a b}', () => {
        assert(!parser.PrimaryExpr.parse('{a b}').status)
      })
      test('allow expressions in values { a: 1+1, ... }', () => {
        const observed = parser.PrimaryExpr.tryParse(`{
          a: 1+1,
          b: x && y ? z : t ? w : u ? v + 2**-2 : 3,
          c: { i: 0, j: 1, k: 2 },
          d: foo ? { n: 123 } : { n: 321 },
        }`)
        const expected = {
          type: 'RecordLiteral',
          pairs: [
            {
              key: 'a',
              val: {
                type: 'BinaryExpr',
                op: '+',
                left: '1',
                right: '1',
              },
            },
            {
              key: 'b',
              val: {
                type: 'CondExpr',
                test: {
                  type: 'BinaryExpr',
                  op: '&&',
                  left: 'x',
                  right: 'y',
                },
                ifYes: 'z',
                ifNo: {
                  type: 'CondExpr',
                  test: 't',
                  ifYes: 'w',
                  ifNo: {
                    type: 'CondExpr',
                    test: 'u',
                    ifYes: {
                      type: 'BinaryExpr',
                      op: '+',
                      left: 'v',
                      right: {
                        type: 'BinaryExpr',
                        op: '**',
                        left: '2',
                        right: { type: 'UnaryExpr', op: '-', arg: '2' },
                      },
                    },
                    ifNo: '3',
                  },
                },
              },
            },
            {
              key: 'c',
              val: {
                type: 'RecordLiteral',
                pairs: [
                  { key: 'i', val: '0' },
                  { key: 'j', val: '1' },
                  { key: 'k', val: '2' },
                ],
              },
            },
            {
              key: 'd',
              val: {
                type: 'CondExpr',
                test: 'foo',
                ifYes: {
                  type: 'RecordLiteral',
                  pairs: [{ key: 'n', val: '123' }],
                },
                ifNo: {
                  type: 'RecordLiteral',
                  pairs: [{ key: 'n', val: '321' }],
                },
              },
            },
          ],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('comma-first', () => {
        const observed = parser.PrimaryExpr.tryParse(
          `{ a: 1
           , b: 2
           , c: 3
           }`)
        const expected = {
          type: 'RecordLiteral',
          pairs: [
            { key: 'a', val: '1' },
            { key: 'b', val: '2' },
            { key: 'c', val: '3' },
          ],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('field names must be valid identifiers', () => {
        assert(!parser.PrimaryExpr.parse('{ invalid__ident: 1 }').status)
        assert(!parser.PrimaryExpr.parse('{ _invalid: 1 }').status)
        assert(!parser.PrimaryExpr.parse('{ $invalid: 1 }').status)
        assert(!parser.PrimaryExpr.parse('{ "not an identifier at all": 1 }').status)
        assert(!parser.PrimaryExpr.parse('{ 5: 1 }').status)
        assert(!parser.PrimaryExpr.parse('{ [1+1]: 1 }').status)
        assert(!parser.PrimaryExpr.parse('{ method() { Return 10 } }').status)
      })
      test('invalid single comma {,}', () => {
        assert(!parser.PrimaryExpr.parse('{,}').status)
        assert(!parser.PrimaryExpr.parse('{ , }').status)
      })
    })
    suite('ArrowFunc', () => {
      test('basic x => x**2', () => {
        const observed = parser.Expression.tryParse('x => x**2')
        const expected = {
          type: 'ArrowFunc',
          params: ['x'],
          body: {
            type: 'BinaryExpr',
            op: '**',
            left: 'x',
            right: '2',
          },
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('looser than CondExpr from the left x => x ? 1 : -1', () => {
        const observed = parser.Expression.tryParse('x => x ? 1 : -1')
        const expected = {
          type: 'ArrowFunc',
          params: ['x'],
          body: {
            type: 'CondExpr',
            test: 'x',
            ifYes: '1',
            ifNo: {
              type: 'UnaryExpr',
              op: '-',
              arg: '1',
            },
          },
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('tighter than CondExpr from the right x ? y => y+1 : y => y+2', () => {
        const observed = parser.Expression.tryParse('x ? y => y+1 : y => y+2')
        const expected = {
          type: 'CondExpr',
          test: 'x',
          ifYes: {
            type: 'ArrowFunc',
            params: ['y'],
            body: {
              type: 'BinaryExpr',
              op: '+',
              left: 'y',
              right: '1',
            },
          },
          ifNo: {
            type: 'ArrowFunc',
            params: ['y'],
            body: {
              type: 'BinaryExpr',
              op: '+',
              left: 'y',
              right: '2',
            },
          },
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('nested CondExpr & ArrowFunc', () => {
        const observed = parser.Expression.tryParse('x ? y => y ? 1 : 2 : y => y ? -1 : -2')
        const expected = {
          type: 'CondExpr',
          test: 'x',
          ifYes: {
            type: 'ArrowFunc',
            params: ['y'],
            body: {
              type: 'CondExpr',
              test: 'y',
              ifYes: '1',
              ifNo: '2',
            },
          },
          ifNo: {
            type: 'ArrowFunc',
            params: ['y'],
            body: {
              type: 'CondExpr',
              test: 'y',
              ifYes: {
                type: 'UnaryExpr',
                op: '-',
                arg: '1',
              },
              ifNo: {
                type: 'UnaryExpr',
                op: '-',
                arg: '2',
              },
            },
          },
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('prohibit arrow func nested in exprs except CondExpr', () => {
        assert(!parser.Expression.parse('- x => x').status)
        assert(!parser.Expression.parse('x + y => y').status)
        assert(!parser.Expression.parse('x || y => y').status)
      })
      test('paren params (x) => 1', () => {
        const observed = parser.Expression.tryParse('(x) => 1')
        const expected = {
          type: 'ArrowFunc',
          params: ['x'],
          body: '1',
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('multiple params (x,y,z) => 1', () => {
        const observed = parser.Expression.tryParse('(x,y,z) => 1')
        const expected = {
          type: 'ArrowFunc',
          params: ['x', 'y', 'z'],
          body: '1',
        }
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
        const expected = {
          type: 'ArrowFunc',
          params: ['x'],
          body: [
            {
              type: 'LetStmt',
              varName: 'y',
              expr: { type: 'BinaryExpr', op: '+', left: 'x', right: '1' },
            },
            {
              type: 'ReturnStmt',
              expr: { type: 'BinaryExpr', op: '*', left: '2', right: 'y' },
            },
          ],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('indented stmt block', () => {
        const indentedParser = parserAtIndent('    ')
        const observed = indentedParser.Expression.tryParse('x => {\n'
          + '        Let y = x + 1\n'
          + '        Return 2*y\n'
          + '    }')
        const expected = {
          type: 'ArrowFunc',
          params: ['x'],
          body: [
            {
              type: 'LetStmt',
              varName: 'y',
              expr: { type: 'BinaryExpr', op: '+', left: 'x', right: '1' },
            },
            {
              type: 'ReturnStmt',
              expr: { type: 'BinaryExpr', op: '*', left: '2', right: 'y' },
            },
          ],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('record literal not allowed as expr w/o parens, same as JS x => { x }', () => {
        assert(!parser.Expression.parse('x => { x }').status)
        const observed = parser.Expression.tryParse('x => ({ x })')
        const expected = {
          type: 'ArrowFunc',
          params: ['x'],
          body: {
            type: 'RecordLiteral',
            pairs: [{ key: 'x', val: 'x' }],
          },
        }
        assert.deepStrictEqual(observed, expected)
      })
    })
  })

  suite('expression operator precedence stack', () => {
    suite('UnaryExpr', () => {
      test('basic -2', () => {
        const observed = parser.UnaryExpr.tryParse('-2')
        const expected = {
          type: 'UnaryExpr',
          op: '-',
          arg: '2',
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('fallthru to PrimaryExpr', () => {
        const observed = parser.UnaryExpr.tryParse('2')
        const expected = '2'
        assert.deepStrictEqual(observed, expected)
      })
    })

    suite('ExponentExpr', () => {
      test('basic 2**3', () => {
        const observed = parser.ExponentExpr.tryParse('2**3')
        const expected = {
          type: 'BinaryExpr',
          op: '**',
          left: '2',
          right: '3',
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('un-op exponent 2**-3', () => {
        const observed = parser.ExponentExpr.tryParse('2**-3')
        const expected = {
          type: 'BinaryExpr',
          op: '**',
          left: '2',
          right: {
            type: 'UnaryExpr',
            op: '-',
            arg: '3',
          },
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('fallthru to UnaryExpr', () => {
        const observed = parser.ExponentExpr.tryParse('-2')
        const expected = {
          type: 'UnaryExpr',
          op: '-',
          arg: '2',
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('prohibit ambiguous -2**2', () => {
        // see comment in ExponentExpr source code for more about this ambiguity
        assert(!parser.ExponentExpr.parse('-2**2').status)

        const observed1 = parser.Expression.tryParse('(-2)**2')
        const expected1 = {
          type: 'BinaryExpr',
          op: '**',
          left: {
            type: 'UnaryExpr',
            op: '-',
            arg: '2',
          },
          right: '2',
        }
        assert.deepStrictEqual(observed1, expected1)

        const observed2 = parser.Expression.tryParse('-(2**2)')
        const expected2 = {
          type: 'UnaryExpr',
          op: '-',
          arg: {
            type: 'BinaryExpr',
            op: '**',
            left: '2',
            right: '2',
          },
        }
        assert.deepStrictEqual(observed2, expected2)
      })
      test('prohibit ambiguous 2**3**2', () => {
        // see comment in ExponentExpr source code for more about this ambiguity
        assert(!parser.ExponentExpr.parse('2**3**2').status)

        const observed1 = parser.Expression.tryParse('(2**3)**2')
        const expected1 = {
          type: 'BinaryExpr',
          op: '**',
          left: {
            type: 'BinaryExpr',
            op: '**',
            left: '2',
            right: '3',
          },
          right: '2',
        }
        assert.deepStrictEqual(observed1, expected1)

        const observed2 = parser.Expression.tryParse('2**(3**2)')
        const expected2 = {
          type: 'BinaryExpr',
          op: '**',
          left: '2',
          right: {
            type: 'BinaryExpr',
            op: '**',
            left: '3',
            right: '2',
          },
        }
        assert.deepStrictEqual(observed2, expected2)
      })
    })

    suite('arithmetic precedence', () => {
      test('MultExpr is left-associative', () => {
        const observed = parser.MultExpr.tryParse('2/3/4')
        const expected = {
          type: 'BinaryExpr',
          op: '/',
          left: {
            type: 'BinaryExpr',
            op: '/',
            left: '2',
            right: '3',
          },
          right: '4',
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('the MDAS (in PEMDAS)', () => {
        const observed = parser.AddExpr.tryParse('2-3*4+5**-6/7**8')
        const expected = {
          type: 'BinaryExpr',
          op: '+',
          left: {
            type: 'BinaryExpr',
            op: '-',
            left: '2',
            right: {
              type: 'BinaryExpr',
              op: '*',
              left: '3',
              right: '4',
            },
          },
          right: {
            type: 'BinaryExpr',
            op: '/',
            left: {
              type: 'BinaryExpr',
              op: '**',
              left: '5',
              right: {
                type: 'UnaryExpr',
                op: '-',
                arg: '6',
              },
            },
            right: {
              type: 'BinaryExpr',
              op: '**',
              left: '7',
              right: '8',
            },
          },
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('okay, PEMDAS', () => {
        const observed = parser.AddExpr.tryParse('2-3*(4+5)**-6/7**8')
        const expected = {
          type: 'BinaryExpr',
          op: '-',
          left: '2',
          right: {
            type: 'BinaryExpr',
            op: '/',
            left: {
              type: 'BinaryExpr',
              op: '*',
              left: '3',
              right: {
                type: 'BinaryExpr',
                op: '**',
                left: {
                  type: 'BinaryExpr',
                  op: '+',
                  left: '4',
                  right: '5',
                },
                right: {
                  type: 'UnaryExpr',
                  op: '-',
                  arg: '6',
                },
              },
            },
            right: {
              type: 'BinaryExpr',
              op: '**',
              left: '7',
              right: '8',
            },
          },
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('fallthru to UnaryExpr', () => {
        const observed = parser.AddExpr.tryParse('-2')
        const expected = {
          type: 'UnaryExpr',
          op: '-',
          arg: '2',
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('fallthru to PrimaryExpr', () => {
        const observed = parser.AddExpr.tryParse('2')
        const expected = '2'
        assert.deepStrictEqual(observed, expected)
      })
    })

    suite('CompareExpr', () => {
      test('basic a != b', () => {
        const observed = parser.CompareExpr.tryParse('a != b')
        const expected = {
          type: 'InequalityExpr',
          left: 'a',
          right: 'b',
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('no chaining a != b != c', () => {
        assert(!parser.CompareExpr.parse('a != b != c').status)
      })
      test('"!=" is mutually exclusive with other comparisons', () => {
        assert(!parser.CompareExpr.parse('a != b < c').status)
        assert(!parser.CompareExpr.parse('a < b != c').status)
      })
      test('chaining', () => {
        const observed = parser.CompareExpr.tryParse('a < b == c <= d < e')
        const expected = {
          type: 'CompareChainExpr',
          chain: [
            {
              type: 'BinaryExpr',
              op: '<',
              left: 'a',
              right: 'b',
            },
            {
              type: 'BinaryExpr',
              op: '==',
              left: 'b',
              right: 'c',
            },
            {
              type: 'BinaryExpr',
              op: '<=',
              left: 'c',
              right: 'd',
            },
            {
              type: 'BinaryExpr',
              op: '<',
              left: 'd',
              right: 'e',
            },
          ],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('improper chaining a < b > c', () => {
        assert(!parser.CompareExpr.parse('a < b > c').status)
      })
      test('chaining starting with equals a == b < c', () => {
        const observed = parser.OrExpr.tryParse('a == b < c')
        const expected = {
          type: 'CompareChainExpr',
          chain: [
            {
              type: 'BinaryExpr',
              op: '==',
              left: 'a',
              right: 'b',
            },
            {
              type: 'BinaryExpr',
              op: '<',
              left: 'b',
              right: 'c',
            },
          ],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('chaining starting with equals a == b > c', () => {
        const observed = parser.OrExpr.tryParse('a == b > c')
        const expected = {
          type: 'CompareChainExpr',
          chain: [
            {
              type: 'BinaryExpr',
              op: '==',
              left: 'a',
              right: 'b',
            },
            {
              type: 'BinaryExpr',
              op: '>',
              left: 'b',
              right: 'c',
            },
          ],
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('fallthru to PrimaryExpr', () => {
        const observed = parser.CompareExpr.tryParse('2')
        const expected = '2'
        assert.deepStrictEqual(observed, expected)
      })
    })

    suite('logical boolean operators && and ||', () => {
      test('&& conventionally has higher precedence than ||', () => {
        const observed = parser.OrExpr.tryParse('a && b || c && d')
        const expected = {
          type: 'BinaryExpr',
          op: '||',
          left: {
            type: 'BinaryExpr',
            op: '&&',
            left: 'a',
            right: 'b',
          },
          right: {
            type: 'BinaryExpr',
            op: '&&',
            left: 'c',
            right: 'd',
          },
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('logical and arithmetic precedence', () => {
        const observed = parser.OrExpr.tryParse('a && b == c > d && e')
        const expected = {
          type: 'BinaryExpr',
          op: '&&',
          left: {
            type: 'BinaryExpr',
            op: '&&',
            left: 'a',
            right: {
              type: 'CompareChainExpr',
              chain: [
                {
                  type: 'BinaryExpr',
                  op: '==',
                  left: 'b',
                  right: 'c',
                },
                {
                  type: 'BinaryExpr',
                  op: '>',
                  left: 'c',
                  right: 'd',
                },
              ],
            },
          },
          right: 'e',
        }
        assert.deepStrictEqual(observed, expected)
      })
    })

    suite('CondExpr', () => {
      test('basic a ? b : c', () => {
        const observed = parser.CondExpr.tryParse('a ? b : c')
        const expected = {
          type: 'CondExpr',
          test: 'a',
          ifYes: 'b',
          ifNo: 'c',
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('precedence with comparisons', () => {
        const observed = parser.CondExpr.tryParse('a == b && c < d < e ? f + 2 : g**3*4')
        const expected = {
          type: 'CondExpr',
          test: {
            type: 'BinaryExpr',
            op: '&&',
            left: {
              type: 'CompareChainExpr',
              chain: [{
                type: 'BinaryExpr',
                op: '==',
                left: 'a',
                right: 'b',
              }],
            },
            right: {
              type: 'CompareChainExpr',
              chain: [
                {
                  type: 'BinaryExpr',
                  op: '<',
                  left: 'c',
                  right: 'd',
                },
                {
                  type: 'BinaryExpr',
                  op: '<',
                  left: 'd',
                  right: 'e',
                },
              ],
            },
          },
          ifYes: {
            type: 'BinaryExpr',
            op: '+',
            left: 'f',
            right: '2',
          },
          ifNo: {
            type: 'BinaryExpr',
            op: '*',
            left: {
              type: 'BinaryExpr',
              op: '**',
              left: 'g',
              right: '3',
            },
            right: '4',
          },
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('nested conditionals', () => {
        const observed = parser.CondExpr.tryParse('a ? b ? c : d ? e : f : g ? h : i')
        const expected = {
          type: 'CondExpr',
          test: 'a',
          ifYes: {
            type: 'CondExpr',
            test: 'b',
            ifYes: 'c',
            ifNo: {
              type: 'CondExpr',
              test: 'd',
              ifYes: 'e',
              ifNo: 'f',
            },
          },
          ifNo: {
            type: 'CondExpr',
            test: 'g',
            ifYes: 'h',
            ifNo: 'i',
          },
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('mis-nested conditionals', () => {
        assert(!parser.CondExpr.parse('a ? b ? c : d ? e : f ? g : h').status)
        assert(!parser.CondExpr.parse('a ? b ? c : d ? e : f : g ? h : i : j').status)
      })
      test('if-elif-elif-else, postfix', () => {
        const observed = parser.CondExpr.tryParse(
          `a ? b :
            c ? d :
            e ? f :
            g`)
        const expected = {
          type: 'CondExpr',
          test: 'a',
          ifYes: 'b',
          ifNo: {
            type: 'CondExpr',
            test: 'c',
            ifYes: 'd',
            ifNo: {
              type: 'CondExpr',
              test: 'e',
              ifYes: 'f',
              ifNo: 'g',
            },
          },
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('if-elif-elif-else, prefix', () => {
        const observed = parser.CondExpr.tryParse(
          `a ? b
          : c ? d
          : e ? f
          : g`)
        const expected = {
          type: 'CondExpr',
          test: 'a',
          ifYes: 'b',
          ifNo: {
            type: 'CondExpr',
            test: 'c',
            ifYes: 'd',
            ifNo: {
              type: 'CondExpr',
              test: 'e',
              ifYes: 'f',
              ifNo: 'g',
            },
          },
        }
        assert.deepStrictEqual(observed, expected)
      })
    })
  })

  suite('Statements', () => {
    suite('LetStmt', () => {
      test('basic Let a = 1', () => {
        const observed = parser.LetStmt.tryParse('Let a = 1')
        const expected = {
          type: 'LetStmt',
          varName: 'a',
          expr: '1',
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('bigger expression', () => {
        const observed = parser.LetStmt.tryParse('Let y = 2*x**3*4')
        const expected = {
          type: 'LetStmt',
          varName: 'y',
          expr: {
            type: 'BinaryExpr',
            op: '*',
            left: {
              type: 'BinaryExpr',
              op: '*',
              left: '2',
              right: {
                type: 'BinaryExpr',
                op: '**',
                left: 'x',
                right: '3',
              },
            },
            right: '4',
          },
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('less whitespace Let x=1+2', () => {
        const observed = parser.LetStmt.tryParse('Let x=1+2')
        const expected = {
          type: 'LetStmt',
          varName: 'x',
          expr: {
            type: 'BinaryExpr',
            op: '+',
            left: '1',
            right: '2',
          },
        }
        assert.deepStrictEqual(observed, expected)
      })
      test('missing whitespace Letx = 1', () => {
        assert(!parser.LetStmt.parse('Letx = 1').status)
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
        const expected = {
          type: 'ArrowFunc',
          params: ['x'],
          body: [
            {
              type: 'LetStmt',
              varName: 'f',
              expr: {
                type: 'ArrowFunc',
                params: ['y'],
                body: [{
                  type: 'ReturnStmt',
                  expr: { type: 'BinaryExpr', op: '+', left: 'y', right: '1' },
                }],
              },
            },
            {
              type: 'LetStmt',
              varName: 'z',
              expr: { type: 'BinaryExpr', op: '+', left: 'x', right: '2' },
            },
            {
              type: 'ReturnStmt',
              expr: {
                type: 'RecordLiteral',
                pairs: [
                  { key: 'z', val: 'z' },
                  { key: 'f', val: 'f' },
                ],
              },
            },
          ],
        }
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
        const expected = {
          type: 'ArrowFunc',
          params: ['x'],
          body: [
            {
              type: 'LetStmt',
              varName: 'f',
              expr: {
                type: 'ArrowFunc',
                params: ['y'],
                body: [{
                  type: 'ReturnStmt',
                  expr: { type: 'BinaryExpr', op: '+', left: 'y', right: '1' },
                }],
              },
            },
            {
              type: 'LetStmt',
              varName: 'z',
              expr: { type: 'BinaryExpr', op: '+', left: 'x', right: '2' },
            },
            {
              type: 'ReturnStmt',
              expr: {
                type: 'RecordLiteral',
                pairs: [
                  { key: 'z', val: 'z' },
                  { key: 'f', val: 'f' },
                ],
              },
            },
          ],
        }
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
})
