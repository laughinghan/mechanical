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
    test('fallback to PrimaryExpr', () => {
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
      assert.throws(() => {
        parser.ExponentExpr.tryParse('-2**2')
      })
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
      const observed = parser.AddExpr.tryParse('2-3*4+5**6/7**8')
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
            right: '6',
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
  })
})
