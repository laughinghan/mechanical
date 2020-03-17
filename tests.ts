import 'mocha'
import assert from 'assert'

import { parser } from './mechc'

suite('Parser', () => {
  test('StateDecl', () => {
    const observed = parser.StateDecl.tryParse('State x = ')
    const expected = { var_name: 'x' }
    assert.deepStrictEqual(observed, expected)
  })

  test('ExponentiationExpr', () => {
    const observed = parser.ExponentiationExpr.tryParse('-2**2')
    const expected = {
      type: 'Exponentiation',
      base: '-2',
      exponent: '2',
    }
    assert.deepStrictEqual(observed, expected)
  })
})
