import 'mocha'
import assert from 'assert'

import { parser } from './mechc'

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
      test('invalid (holes) [1,,2]', () => {
        assert(!parser.PrimaryExpr.parse('[ 1, , 2 ]').status)
        assert(!parser.PrimaryExpr.parse('[ 1, 2,, ]').status)
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
      })
      test('prohibit ambiguous 2**3**2', () => {
        // see comment in ExponentExpr source code for more about this ambiguity
        assert(!parser.ExponentExpr.parse('2**3**2').status)
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
})
