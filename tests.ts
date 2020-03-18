import 'mocha'
import assert from 'assert'

import { parser } from './mechc'

suite('Parser', () => {
  test('StateDecl', () => {
    const observed = parser.StateDecl.tryParse('State x = ')
    const expected = { var_name: 'x' }
    assert.deepStrictEqual(observed, expected)
  })

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
})
