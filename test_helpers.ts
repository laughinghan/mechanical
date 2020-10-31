// fast-check "arbitraries" to generate type values

import { match } from 'assert'
import fc from 'fast-check'
import { Types } from './mechc'


// if we randomly generate actual identifiers, they'll practically never
// coincide, and we'll never get products or sums that are subtypes of
// one another
export const arb_ident = fc.constantFrom('foo', 'bar', 'baz', 'qux')

// currently unused. TODO: remove?
export const arb_type = (w: number) => fc.oneof<fc.Arbitrary<Types.Type>[]>(
  fc.constant('Anything'),
  fc.constant('Nothing'),
  arb_nontrivial_type(w),
)

export const arb_nontrivial_type: fc.Memo<Types.NontrivialType> = fc.memo(w =>
  w <= 1 ? arb_scalar(w) :
  fc.frequency<fc.WeightedArbitrary<Types.NontrivialType>[]>(
    { weight: 4, arbitrary: arb_primitive(w) },
    { weight: 3, arbitrary: arb_composite(w) },
    { weight: 1, arbitrary: arb_function(w) },
  )
)

export const arb_primitive = fc.memo(w =>
  fc.frequency<fc.WeightedArbitrary<Types.PrimitiveType>[]>(
    { weight: 1, arbitrary: fc.constant('None') },
    {
      weight: 1,
      arbitrary: fc.record<Types.Err>({
        species: fc.constant('Error'),
        error: arb_sum(w),
        nullable: fc.frequency(
          { weight: 1, arbitrary: fc.constant(true as const) },
          { weight: 4, arbitrary: fc.constant(undefined) },
        ),
      }),
    },
    { weight: 6, arbitrary: arb_scalar(w) },
  )
)

const arb_errorable = <T extends Types.NontrivialType>(
  w: number,
  record: {[K in Exclude<keyof T, 'nullable' | 'error'>]: fc.Arbitrary<T[K]>},
): fc.Arbitrary<T> => fc.record<T>({
  ...record,
  nullable: fc.frequency(
    { weight: 1, arbitrary: fc.constant(true as const) },
    { weight: 4, arbitrary: fc.constant(undefined) },
  ),
  error: (w <= 1?
    fc.constant(undefined) :
    fc.frequency(
      { weight: 10000, arbitrary: fc.constant(undefined) },
      { weight: w*w, arbitrary: arb_sum(w/2|0) },
    )
  ),
} as any)

export const arb_scalar = fc.memo(w => arb_errorable<Types.ScalarType>(w, {
  species: fc.constantFrom(...['boolean', 'number', 'string'] as const),
}))

export const arb_composite = fc.memo(w =>
  fc.oneof(arb_array(w), arb_product(w), arb_sum(w)))

export const arb_array = fc.memo(w => arb_errorable<Types.Arr>(w, {
  species: fc.constant('Array'),
  ItemType: arb_nontrivial_type(w - 10),
}))

function weighted_dict<Arb extends fc.Arbitrary<any>>(
  w: number, arb_value: (w: number) => Arb
) {
  const nonempty_dict = (new_w: number) =>
    fc.dictionary(arb_ident, arb_value(new_w))
    .filter(fields => Object.keys(fields).length > 0)
  return fc.frequency(
    { weight: 4, arbitrary: nonempty_dict(w - 10)
        .map(fields => {
          for (const f of Object.keys(fields).slice(1)) { // trim to 1
            delete fields[f]
          }
          return fields
        }) },
    { weight: 4, arbitrary: nonempty_dict(w - 20)
        .map(fields => {
          for (const f of Object.keys(fields).slice(2)) { // trim to 2
            delete fields[f]
          }
          return fields
        }) },
    { weight: 1, arbitrary: nonempty_dict(w/2|0)
        .map(fields => {
          const fieldNames = Object.keys(fields)
          for (const f of fieldNames.slice(Math.ceil(fieldNames.length/2))) { // halve
            delete fields[f]
          }
          return fields
        }) },
    { weight: 1, arbitrary: nonempty_dict(w/4|0) }, // full dict
  )
}

export const arb_product = fc.memo(w => arb_errorable<Types.Product>(w, {
  species: fc.constant('Product'),
  fields: weighted_dict(w, arb_nontrivial_type),
}))

export const arb_sum = fc.memo(w => arb_errorable<Types.Sum>(w, {
  species: fc.constant('Sum'),
  variants: weighted_dict(w, w => fc.option(arb_nontrivial_type(w))),
}))

export const arb_function = fc.memo(w => fc.oneof(
  arb_errorable<Types.FuncUnary>(w, {
    species: fc.constant('Function'),
    paramCount: fc.constant(1),
    result: arb_nontrivial_type(w/2|0),
    param: arb_nontrivial_type(w/2|0),
  }),
  arb_errorable<Types.FuncBinary>(w, {
    species: fc.constant('Function'),
    paramCount: fc.constant(2),
    result: arb_nontrivial_type(w/3|0),
    param1: arb_nontrivial_type(w/3|0),
    param2: arb_nontrivial_type(w/3|0),
    param2Optional: fc.constantFrom(true as const, undefined),
  }),
  fc.record({
    func: arb_errorable<Types.FuncNary>(w, {
      species: fc.constant('Function'),
      paramCount: fc.constant(99),
      result: arb_nontrivial_type(w/5|0),
      firstParam: arb_nontrivial_type(w/5|0),
      labels: fc.constant([]),
      params: fc.constant({}),
      optionalCount: fc.constant(0),
    }),
    paramKVs: fc.array(
      fc.record({
        label: arb_ident,
        param: arb_nontrivial_type(w/5|0),
      }),
      { minLength: 2, maxLength: 4 },
    ),
    optionalCount: fc.nat(),
  })
  .map(({ func, paramKVs, optionalCount }) => {
    const params: { [label: string]: Types.NontrivialType } = {}
    for (const { label, param } of paramKVs) {
      params[label] = param
    }
    const labels = Object.keys(params)
    return {
      ...func, labels, params,
      optionalCount: optionalCount % labels.length
    }
  }),
))

export function isWellFormed(T: Types.Type, context?: () => {}): true {
  function makeError(msg: string) {
    const contextStr = context ? '\n' + JSON.stringify(context(), null, 2) : ''
    try {
      return new Error(msg + ':\n' + Types.stringify(T) + '\n' + JSON.stringify(T, null, 2) + contextStr)
    } catch {
      try {
        return new Error(msg + ':\n(Could not Types.stringify())\n' + JSON.stringify(T, null, 2) + contextStr)
      } catch {
        return new Error(msg + ':\n(Could not Types.stringify())\n(Could not JSON.stringify())' + contextStr)
      }
    }
  }

  if (!T) throw makeError('falsy')
  if (T === 'Anything' || T === 'Nothing' || T === 'None') return true

  if (T.nullable !== true && T.nullable !== undefined) {
    throw makeError('invalid nullable value')
  }
  if (T.species === 'Error') return isWellFormed(T.error, context)
  if (T.error !== undefined) isWellFormed(T.error, context)

  switch (T.species) {
    case 'boolean':
    case 'number':
    case 'string':
      return true
    case 'Array':
      return isWellFormed(T.ItemType, context)
    case 'Product':
      const fieldNames = Object.keys(T.fields)
      if (fieldNames.length === 0) throw makeError('record has no fields')
      return fieldNames.every(field => isWellFormed(T.fields[field], context)) as true
    case 'Sum':
      const tags = Object.keys(T.variants)
      if (tags.length === 0) throw makeError('sum type has no variants')
      return tags.every(tag => {
        const variant = T.variants[tag]
        return variant === null || isWellFormed(variant, context)
      }) as true
    case 'Function':
      isWellFormed(T.result, context)
      if (T.paramCount === 1) return isWellFormed(T.param, context)
      if (T.paramCount === 2) {
        if (T.param2Optional !== true && T.param2Optional !== undefined) {
          throw makeError('invalid param2Optional value')
        }
        return isWellFormed(T.param1, context) && isWellFormed(T.param2, context)
      }
      if (T.paramCount !== 99) throw makeError('invalid paramCount')
      isWellFormed(T.firstParam, context)
      if (T.optionalCount < 0 || T.optionalCount > T.labels.length
        || (T.optionalCount|0) !== T.optionalCount
      ) {
        throw makeError('invalid optionalCount')
      }
      if (Object.keys(T.params).length !== T.labels.length) {
        throw makeError('labels and params don\'t match')
      }
      return T.labels.every(label => isWellFormed(T.params[label], context)) as true
  }
  throw makeError('invalid species')
}

export function arb_type_pairs<Pair>(T: fc.Arbitrary<Types.NontrivialType>, filterMap: (T: Types.NontrivialType, U: Types.NontrivialType) => Pair | undefined, n = 30): fc.Arbitrary<Pair[]> {
  return arb_similar_types(n, T, true)
    .map(Ts => {
      const matches: Pair[] = []
      for (let i = 1; i < Ts.length; i += 1) {
        for (let j = 0; j < i; j += 1) {
          const pair = filterMap(Ts[i], Ts[j])
          if (pair) matches.push(pair)
        }
      }
      return matches
    })
    .filter(matches => matches.length > 1)
}

const skewedStrings = fc.tuple(
  fc.nat({max: 10}), fc.nat({max: 10}),
  fc.hexaString(),   fc.hexaString({minLength:1}),
)
.map(([n, m, a, b]) => (a+b).slice(0, 1+n+m))

export function arb_similar_types<t extends Types.CompositeType>(n: number, T: fc.Arbitrary<t>, preserve: true): fc.Arbitrary<t[]>
export function arb_similar_types(n: number, T: fc.Arbitrary<Types.NontrivialType>, preserve?: true): fc.Arbitrary<Types.NontrivialType[]>
export function arb_similar_types(n: number, T: fc.Arbitrary<Types.NontrivialType>, preserve?: true): fc.Arbitrary<Types.NontrivialType[]> {
  return fc.record({
    T,
    rngs: fc.array(
      fc.record({
        str: skewedStrings, // reproducible RNGs
        type: arb_nontrivial_type(5), // randomized type that can be inserted
      }),
      { minLength: n, maxLength: n}),
  })
  .map(({T, rngs}) => {
    return rngs.map(({str, type}) => {
      return T = perturb_type(T, str, type, preserve)
    })
  })
}

// make one small random modification to a type
// Named by analogy to "perturbation theory" from physics
// Without this, randomly generated types are almost never subtypes of each other,
// which makes tests pretty useless
export function perturb_type<t extends Types.CompositeType>(T: t, rng: string, U: Types.NontrivialType, preserve: true): t
export function perturb_type(T: Types.NontrivialType, rng: string, U: Types.NontrivialType, preserve?: true): Types.NontrivialType
export function perturb_type(T: Types.NontrivialType, rng: string, U: Types.NontrivialType, preserve?: true): Types.NontrivialType {
  if (T === 'None') return U

  const randHex = (i: number) => parseInt(rng.charAt(i), 16)
  const randIdent = (i: number) =>
    'zig zag zot xyzzy'.split(' ')[randHex(i) % 4]

  switch (T.species) {
    case 'Error':
      // if we're at the end of the rng
      if (rng.length < 3) {
        // 1/4 of the time, replace with U
        if (randHex(0) % 4 === 1) return U
        // 3/4 of the time, invert .nullable
        return { ...T, nullable: !T.nullable || undefined }
      }
      // otherwise, perturb the sum
      return { ...T, error: perturb_type(T.error, rng.slice(1), U, true) }
    case 'boolean':
    case 'number':
    case 'string':
      // if we have enough rng left, and there is an error, then
      // 3/4 of the time, perturb the error sum
      if (rng.length > 2 && T.error && randHex(0) > 3) {
        return { ...T, error: perturb_type(T.error, rng.slice(1), U, true) }
      }

      // otherwise (if we're at the end of the rng, or there's no error) then
      // invert .nullable, replace with U, or mutate into another scalar type
      const species = 'boolean number string'.split(' ')[randHex(0) % 4]
      if (!species) return U
      if (species === T.species) {
        return { ...T, nullable: !T.nullable || undefined }
      }
      return { ...T, species: species as typeof T.species}
    case 'Array':
      // if we're at the end of the rng,
      if (rng.length < 3) {
        const rand = randHex(0) % 8
        // if there is an error, 1/8 of the time, nullify the error
        if (T.error && rand === 0) return { ...T, error: undefined }
        // 1/8 of the time, invert .nullable
        if (rand === 1) return { ...T, nullable: !T.nullable || undefined }
        // if we don't have to preserve the composite type,
        if (!preserve) {
          // 1/8 of the time, replace the array with U
          if (rand === 2) return U
          // 1/8 of the time, unwrap the array type
          if (rand === 3) return T.ItemType
        }
        // otherwise, 1/2 of the time, replace item type with U, returning U[]
        return { ...T, ItemType: U }
      }
      // otherwise, go deeper to perturb.
      // 1/3 of the time, perturb the error sum
      if (T.error && randHex(0) % 3 === 1) {
        return { ...T, error: perturb_type(T.error, rng.slice(1), U, true) }
      }
      // 2/3 of the time, perturb the item type
      return { ...T, ItemType: perturb_type(T.ItemType, rng.slice(1), U) }
    case 'Product': {
      // if we're at the end of the rng,
      if (rng.length < 3) {
        const rand16 = randHex(0)
        // if there is an error, 1/16 of the time, nullify the error
        if (T.error && rand16 === 0) return { ...T, error: undefined }
        // 1/16 of the time, invert .nullable
        if (rand16 === 1) return { ...T, nullable: !T.nullable || undefined }

        const fieldNames = Object.keys(T.fields)
        // if we don't have to preserve the composite type, then
        // 1/8 of the time, replace with U or a random field type
        if (!preserve && rand16 >> 1 === 1) {
          // there may only be one hex character left of the rng string
          // so use the last hex character
          const i = randHex(rng.length - 1) % (fieldNames.length + 1)
          if (i === fieldNames.length) return U
          const randField = fieldNames[i]
          return T.fields[randField]
        }
        // there may only be one hex character left of the rng string
        // so use the last hex character
        const i = randHex(rng.length - 1) % fieldNames.length
        const randField = fieldNames[i]

        let rand4 = rand16 >> 2
        // 1/4 of the time, delete a random field
        if (rand4 === 1) {
          // unless there's only 1 field, then can't delete, so
          // 1/2:1/2 randomly add or replace a field
          if (fieldNames.length === 1) {
            rand4 = rand16 % 2 ? 2 : 3
          } else {
            const fields = { ...T.fields }
            delete fields[randField]
            return { ...T, fields }
          }
        }
        // 1/4 of the time, add U as a random field
        if (rand4 === 2) return { ...T,
          fields: { ...T.fields, [randIdent(rng.length - 1)]: U } }
        // otherwise, 1/4 of the time, replace a random field type with U
        return { ...T, fields: { ...T.fields, [randField]: U } }
      }
      // otherwise, go deeper to perturb.
      // Pick at random a field or the error sum (if present) to perturb
      const fieldNames = Object.keys(T.fields)
      const i = randHex(0) % (fieldNames.length + (T.error ? 1 : 0))
      if (i === fieldNames.length && T.error) {
        return { ...T, error: perturb_type(T.error, rng.slice(1), U, true) }
      }
      const randField = fieldNames[i]
      return { ...T, fields: { ...T.fields, [randField]:
        perturb_type(T.fields[randField], rng.slice(1), U) } }
    }
    case 'Sum': {
      // if we're at the end of the rng,
      if (rng.length < 3) {
        const rand16 = randHex(0)
        // if there is an error, 1/16 of the time, nullify the error
        if (T.error && rand16 === 0) return { ...T, error: undefined }
        // 1/16 of the time, invert .nullable
        if (rand16 === 1) return { ...T, nullable: !T.nullable || undefined }

        const tags = Object.keys(T.variants)

        // if we don't have to preserve the composite type, then
        // 1/8 of the time, replace with U or a random variant type
        if (!preserve && rand16 >> 1 === 1) {
          // there may only be one hex character left of the rng string
          // so use the last hex character
          const i = randHex(rng.length - 1) % (tags.length + 1)
          if (i === tags.length) return U
          const randTag = tags[i]
          return T.variants[randTag] || U
        }
        // there may only be one hex character left of the rng string
        // so use the last hex character
        const i = randHex(rng.length - 1) % tags.length
        const randTag = tags[i]

        let rand4 = rand16 >> 2
        // 1/4 of the time, delete a random variant
        if (rand4 === 1) {
          // unless there's only 1 variant, then can't delete, so
          // 1/2:1/2 randomly add or replace a variant
          if (tags.length === 1) {
            rand4 = rand16 % 2 ? 2 : 3
          } else {
            const variants = { ...T.variants }
            delete variants[randTag]
            return { ...T, variants }
          }
        }
        // 1/4 of the time, add U as a random variant
        if (rand4 === 2) return { ...T,
          variants: { ...T.variants, [randIdent(rng.length - 1)]: U } }
        // otherwise, 1/4 of the time, replace a random variant with U
        return { ...T,
          variants: { ...T.variants, [randTag]: U } }
      }
      // otherwise, go deeper to perturb.
      // Pick at random a variant or the error sum (if present) to perturb
      const tags = Object.keys(T.variants)
      const i = randHex(0) % (tags.length + (T.error ? 1 : 0))
      if (T.error && (i === tags.length || T.variants[tags[i]] === null)) {
        return { ...T, error: perturb_type(T.error, rng.slice(1), U, true) }
      }
      const randTag = tags[i]
      const randVariant = T.variants[randTag]
      return { ...T, variants: { ...T.variants, [randTag]:
        randVariant ? perturb_type(randVariant, rng.slice(1), U) : U } }
    }
    case 'Function': {
      // if we're at the end of the rng,
      if (rng.length < 3) {
        const rand16 = randHex(0)
        // if there is in an error 1/16 of the time, nullify the error
        if (T.error && rand16 === 0) return { ...T, error: undefined  }
        // 1/16 of the time, invert .nullable
        if (rand16 === 1) return { ...T, nullable: !T.nullable || undefined }

        // for unary functions,
        if (T.paramCount === 1) {
          // if we don't have to preserve the composite type, then
          // 1/8 of the time, replace with U or a random param/result type
          if (!preserve && rand16 >> 1 === 1) {
            const i = randHex(rng.length - 1) % 3
            if (i === 0) return T.result
            if (i === 1) return T.param
            return U
          }
          // 1/4 of the time, unary -> binary
          if (rand16 >> 2 === 1) {
            if (rand16 % 2) {
              return { ...T, paramCount: 2, param1: T.param, param2: U,
                param2Optional: (rand16 >> 1) % 2 ? true : undefined }
            } else {
              return { ...T, paramCount: 2, param1: U, param2: T.param,
                param2Optional: (rand16 >> 1) % 2 ? true : undefined }
            }
          }
          // 1/4 of the time, unary -> n-ary with only one labeled parameter
          if (rand16 >> 2 === 2) {
            const label = randIdent(rng.length - 1)
            return { ...T, paramCount: 99,
              firstParam: rand16 % 2 ? T.param : U, labels: [label],
              params: { [label]: rand16 % 2 ? U : T.param },
              optionalCount: (rand16 >> 1) % 2 }
          }
          // otherwise, 1/4 of the time, replace a random param with U
          return rand16 % 2 ? { ...T, param: U } : { ...T, result: U }
        }
        // for binary functions,
        if (T.paramCount === 2) {
          // if we don't have to preserve the composite type, then
          // 1/16 of the time, replace with U or a random param/result type
          if (!preserve && rand16 === 2) {
            const i = randHex(rng.length - 1) % 4
            if (i === 0) return T.result
            if (i === 1) return T.param1
            if (i === 2) return T.param2
            return U
          }
          // 1/16 of the time (or 1/8 if we have to preserve the composite
          // type), twiddle the optionality of the 2nd parameter
          if (rand16 >> 1 === 1) {
            return { ...T, param2Optional: !T.param2Optional || undefined }
          }
          // 1/8 of the time, swap two param/result types
          if (rand16 >> 1 === 2) {
            // We don't have a lot of entropy to work with, but the entropy
            // in a single swap isn't that much, it's N*M where N, the
            // possible indices of the item to swap, equals the length of
            // the array, and M, the possible indices of items with which
            // to swap, equals the array.length - 1.
            // So we sample from N by doing random() % N, then we sample
            // independently from M by doing Math.floor(random()/N) % M.
            // Here, N = 3 and M = 2
            const rand = randHex(rng.length - 1)
            const toSwap = rand % 3
            const swapWithRand = (rand/3|0) % 2
            const swapWith = swapWithRand + (swapWithRand >= toSwap ? 1 : 0)
            const paramTypes = [T.param1, T.param2, T.result]
            ;[paramTypes[toSwap], paramTypes[swapWith]] =
              [paramTypes[swapWith], paramTypes[toSwap]]
            return { ...T, param1: paramTypes[0], param2: paramTypes[1],
              result: paramTypes[2] }
          }
          // 1/4 of the time, binary -> unary
          if (rand16 >> 2 === 2) {
            return { ...T, paramCount: 1,
              param: rand16 % 2 ? T.param1 : T.param2 }
          }
          // 1/4 of the time, binary -> n-ary
          if (rand16 >> 2 === 3) {
            return { ...T, paramCount: 99, firstParam: T.param1,
              labels: ['zig', 'zag'], params: { zig: T.param2, zag: U },
              optionalCount: randHex(rng.length - 1) % 3 }
          }
          // otherwise, 1/8 of the time, replace a random param with U
          const i = randHex(rng.length - 1) % 3
          if (i === 0) return { ...T, param1: U }
          if (i === 1) return { ...T, param2: U }
          return { ...T, result: U }
        }
        // for n-ary functions,
        if (T.paramCount === 99) {
          // if we don't have to preserve the composite type, then
          // 1/16 of the time, replace with U or a random param/result type
          if (!preserve && rand16 === 2) {
            const i = randHex(rng.length - 1) % (T.labels.length + 3)
            if (i === T.labels.length + 2) return U
            if (i === T.labels.length + 1) return T.result
            if (i === T.labels.length)     return T.firstParam
            return T.params[T.labels[i]]
          }
          // 1/16 of the time (or 1/8 if we have to preserve the composite
          // type), randomize the optionalCount
          if (rand16 >> 1 === 1) {
            return { ...T,
              optionalCount: randHex(rng.length - 1) % (T.labels.length + 1) }
          }
          // 1/8 of the time, swap two param/result types
          if (rand16 >> 1 === 2) {
            // We don't have a lot of entropy to work with, but the entropy
            // in a single swap isn't that much, it's N*M where N, the
            // possible indices of the item to swap, equals the length of
            // the array, and M, the possible indices of items with which
            // to swap, equals the array.length - 1.
            // So we sample from N by doing random() % N, then we sample
            // independently from M by doing Math.floor(random()/N) % M.
            // Here, N = T.labels.length + 2 and M = T.labels.length + 1
            const rand = randHex(rng.length - 1)
            const toSwap = rand % (T.labels.length + 2)
            const swapWithRand = (rand/(T.labels.length + 2)|0) % (T.labels.length + 1)
            const swapWith = swapWithRand + (swapWithRand >= toSwap ? 1 : 0)
            const paramTypes = [T.result, T.firstParam].concat(
              T.labels.map(label => T.params[label]))
            ;[paramTypes[toSwap], paramTypes[swapWith]] =
              [paramTypes[swapWith], paramTypes[toSwap]]
            const params: {[label: string]: Types.NontrivialType} = {}
            for (let i = 0; i < T.labels.length; i += 1) {
              params[T.labels[i]] = paramTypes[i + 2]
            }
            return { ...T,
              result: paramTypes[0], firstParam: paramTypes[1], params }
          }
          // 1/8 of the time, swap two labels (if there are at least two)
          if (rand16 >> 1 === 3 && T.labels.length > 1) {
            // here, N = T.labels.length and M = T.labels.length - 1
            const rand = randHex(rng.length - 1)
            const toSwap = rand % T.labels.length
            const swapWithRand = (rand/T.labels.length|0) % (T.labels.length - 1)
            const swapWith = swapWithRand + (swapWithRand >= toSwap ? 1 : 0)
            const labels = T.labels.slice() // copy array
            ;[labels[toSwap], labels[swapWith]] =
              [labels[swapWith], labels[toSwap]]
            return { ...T, labels }
          }
          // 1/4 of the time, delete a random param
          if (rand16 >> 2 === 2) {
            if (T.labels.length === 1) { // n-ary -> unary
              return { ...T, paramCount: 1,
                param: rand16 % 2 ? T.firstParam : T.params[T.labels[0]] }
            }
            const i = randHex(rng.length - 1) % T.labels.length
            const randLabel = T.labels[i]
            // if there were 3 params, 1/8 of the time, n-ary -> binary
            if (T.labels.length === 2 && rand16 % 2) {
              return { ...T, paramCount: 2, param1: T.firstParam,
                param2: T.params[randLabel],
                param2Optional: i >= 2-T.optionalCount ? true : undefined }
            }
            // the other 1/8 of the time, just delete a param
            const labels = T.labels.slice() // copy array
            labels.splice(i, 1)
            const params = { ...T.params }
            delete params[randLabel]
            return { ...T, labels, params, optionalCount:
              T.optionalCount - +(i >= T.labels.length - T.optionalCount) }
          }
          // 1/8 of the time, add U as a random param
          if (rand16 >> 1 === 6) {
            const randLabel = randIdent(rng.length - 1)
            // if randomly-generated label is already there, just replace it
            if (T.params[randLabel]) {
              return { ...T, params: { ...T.params, [randLabel]: U } }
            }
            const i = randHex(rng.length - 1) % (T.labels.length + 1)
            const labels = T.labels.slice() // copy array
            labels.splice(i, 0, randLabel)
            return { ...T, labels, params: { ...T.params, [randLabel]: U } }
          }
          // otherwise, 1/8 of the time, replace a random param/result type with U
          const i = randHex(rng.length - 1) % (T.labels.length + 2)
          if (i === T.labels.length + 1) return { ...T, result: U }
          if (i === T.labels.length)     return { ...T, firstParam: U }
          const randLabel = T.labels[i]
          return { ...T,
            params: { ...T.params, [randLabel]: T.params[randLabel] } }
        }
      }
      // otherwise, go deeper to perturb.
      // Pick a random variant or the error sum (if present) perturb
      const numParams = T.paramCount === 99 ? T.labels.length+2 : T.paramCount+1
      if (T.error && randHex(0) % (numParams + 1) === numParams) {
        return { ...T, error: perturb_type(T.error, rng.slice(1), U, true) }
      }
      if (T.paramCount === 1) {
        return randHex(0) >> 3
          ? { ...T, param: perturb_type(T.param, rng.slice(1), U) }
          : { ...T, result: perturb_type(T.result, rng.slice(1), U) }
      }
      if (T.paramCount === 2) {
        const i = randHex(0) % 3
        return !i ? { ...T, result: perturb_type(T.result, rng.slice(1), U) }
          : i === 1 ? { ...T, param1: perturb_type(T.param1, rng.slice(1), U) }
          : { ...T, param2: perturb_type(T.param2, rng.slice(1), U) }
      }
      const i = randHex(0) % numParams
      if (i === T.labels.length + 1) return { ...T,
        result: perturb_type(T.result, rng.slice(1), U) }
      if (i === T.labels.length) return { ...T,
        firstParam: perturb_type(T.firstParam, rng.slice(1), U) }
      const randLabel = T.labels[i]
      return { ...T, params: { ...T.params,
        [randLabel]: perturb_type(T.params[randLabel], rng.slice(1), U) } }
    }
  }
}