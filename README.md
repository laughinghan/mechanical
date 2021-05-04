# Mechanical [![Chat on Gitter](https://badges.gitter.im/mechanical-lang/community.svg)][Gitter]

[Gitter]: https://gitter.im/mechanical-lang/community

A language that makes building webapps (and more!) as easy as building
finite-state machines.

- **Best-of-each-world:**
  + Define a state machine **declaratively** using **purely functional** logic,
    perform side-effects **imperatively**.
  + **Statically typed, no type annotations.** Algebraic data types without
    type declarations.
- **X% faster and Y% smaller than React:**
  Instead of virtual DOM diffing, just compile to imperative, mutative
  JavaScript.
- **Fully interoperable:**
  Compile to JavaScript modules that can call or be called by any JS library
  or browser/Node API.

(Comparisons with: [Elm](#elm), [Flux](#flux), [Redux](#redux), or
 [Functional Core, Imperative Shell](#functional-core-imperative-shell))

**Status:** First compile and run of Hello World works—and absolutely
nothing else. Help wanted!


## Show Me Some Code Already

Okay.

```
State counter = 0

// Declarative JSX-like view syntax
%View '#app':
  <div>
    <p>Counter: {counter}</p>
    <button @increment>+1</button>
  </div>

// Event handlers imperatively update state
When increment.Click:
  Change counter to (Current counter + 1)
```

That compiles to this fast, mutative (yet clean & readable) JavaScript:

```jsx
var counter = 0

const p = <p>Counter: {counter}</p>
const increment_button = <button>+1</button>
render_view('#app',
  <div>
    {p}
    {increment_button}
  </div>
)

const increment = {
  Click: event_stream(increment_button, 'click'),
}

increment.Click.subscribe(() => {
  counter += 1
  p.textContent = 'Counter: ' + counter
  // ^ direct DOM manipulation, faster than
  //   virtual DOM diffing like React
})
```

(okay, that's theoretical, that doesn't actually work yet)

## Usage

Install:

```
npm install mechanical
```

Run the compiler:

```
npx mechc some_file.mech
```

Which will output: `some_file.js`

## Differences from JS

#### Extensions

Mechanical-specific extensions to JS expression syntax (which only make sense
with Mechanical semantics, and wouldn't make sense for JS:
- (TODO) `cmd:` blocks
- (TODO) Hashtagged values `#tag value` and pattern-matching `match tagged {...}`

      Let x = Current switch_state ? #on 72 : #off "sleeping for the night"
      // expression-form pattern-matching:
      Let y = match x { #on temp -> temp; #off message -> 68 }
      // statement-form pattern-matching:
      Match x:
          #on temp ->
              Change color to #green
              Change temperature_dial to temp
          #off message ->
              Change display_message to message.slice(from: 0, to: 100)

- (TODO) `is` and `isnt` pattern-matching operators
    + `matches` makes more sense than `is`, but what's the inverse?
      `doesnt_match`?
- (TODO) Macros: the `%` prefix, like `%View` and `%map{}`, is eventually
  intended to connote macros aka userland syntax extensions, although for now
  they'll be hardcoded into the compiler

Extensions to JavaScript features:
- Multiline strings
    + Both string literals (`""` and `''`) can have newlines in them, but
      subsequent lines must be indented to at least the same level as the
      open-quote; that indentation is then omitted from the result:

          When button.Click:
              Let valid_string = "first line
              second line
                third line" // == "first line\nsecondline\n  third line"

          When button2.Click:
              Let invalid_string = "first line
            second line" // error!

- (TODO) Alternate string interpolation syntax ``` $`text {expression} text` ```
    + In addition to JS [template literal syntax] (e.g.
      ``` `text ${expression} text` ```), Mechanical supports an alternative
      string interpolation syntax where you prefix the string with `$`:

          $`text {expression} text`
          $'text {expression} text'
          $"text {expression} text"
          // are all equivalent to:
          `text ${expression} text`

      This is similar to C# but without support for format specifiers.

- postfix/infix function calls, named parameters

      // there are no 0-argument functions (functions can't have side-effects,
      //                                    so what would be the point?)

      // 1-argument functions can be called prefix or postfix.
      // These are exactly equivalent:
      foo(x)
      x.foo()

      // 2-argument functions can be called prefix or infix.
      // These are exactly equivalent:
      bar(x, y)
      x.bar(y)

      // for 3-or-more-argument functions, labels are required for parameters
      // after the first parameter.
      // They can also be called prefix or infix, so these are equivalent:
      qux(x, param: y, another: z)
      x.qux(param: y, another: z)

      // e.g.
      list.reduce(from: initial_value, update: (so_far, next) => ...)

- (TODO) Tilde functions like `~ + 1`, even lighter-weight than arrow functions:

      // I think of the ~ as like a scribble and read it as "thing"
      // so I read (~ + 1) as "thing plus one"

      [1, 2, 3].map(~ + 1) == [2, 3, 4]
      [1, 2, 3].map(2*~)   == [2, 4, 6]

      // basically, the tilde function expands "outward" to encompass built-in
      // ops (arithmetic, string templating, boolean), and expands "rightward"
      // to encompass .prop property accesses and .foo() postfix and infix
      // function calls

      // basic expressions:
      ~ + 1                 // equivalent to x => x + 1
      -1/(2*~)              // equivalent to x => -1/(2*x)
      `Hello, ${~}!`        // equivalent to x => `Hello, ${x}!`
      ~ < 0 || ~ > 100      // equivalent to x => x < 0 || x > 100
      ~.prop                // equivalent to x => x.prop
      ~.x**2 + ~.y**2       // equivalent to p => p.x**2 + p.y**2
      #tag ~                // equivalent to x => #tag x
      ~.#tag()              // equivalent to x => #tag x
      [~]                   // equivalent to x => [x]
      { prop: ~ }           // equivalent to prop => ({ prop })

      // function calls:
      foo(~, 1)             // equivalent to x => foo(x, 1)
      ~.foo(1)              // equivalent to x => foo(x, 1)
      foo(1, a: ~, b: 2)    // equivalent to x => foo(1, a: x, b: 2)
      1/sqrt(~)             // equivalent to x => 1/sqrt(x)
      Do foo(~)             // equivalent to x => Do foo(x)
      (2*~).foo()           // equivalent to x => foo(2*x)
      ~.foo().prop          // equivalent to x => foo(x).prop
      ~.foo().Do!           // equivalent to x => Do foo(x)
      ~.foo().bar().qux()   // equivalent to x => qux(bar(foo(x)))

      // 2-argument tilde function:
      ~ + ~~                // equivalent to (x, y) => x + y
      ~~**~                 // equivalent to (e, b) => b**e
      foo(~~, ~)            // equivalent to (x, y) => foo(y, x)

      // !!! Special Exception: rearranging arguments !!!
      x.foo(1, a: ~, b: 2)  // equivalent to foo(1, a: x, b: 2)
                            //        [Note] ^ not an anonymous function
      ~.foo(1, a: ~, b: 2)  // equivalent to x => foo(1, a: x, b: 2)

      // invalid:
      foo(~)                // redundant, just use foo
      ~.foo()               // redundant, just use foo
      foo(~, ~~)            // redundant, just use foo
      ~.foo(~~)             // redundant, just use foo
      x.foo(~)              // ambiguous: foo(x) or y => foo(x, y) ?
                            // Just use foo(x, ~)
      x.foo(a: 1, b: ~)     // instead use foo(x, a: 1, b: ~)
      #tag foo(1, ~)        // ambiguous: #tag (x => foo(1, x))
                            //         or x => #tag foo(1, x) ?
                            // instead use ~.foo(1, ~).#tag()
      [ foo(1, ~) ]         // ambiguous: [ (x => foo(1, x) ]
                            //         or x => [ foo(1, x) ] ?
      { prop: foo(1, ~) }   // ambiguous: { prop: (x => foo(1, x)) }
                            //         or x => ({ prop: foo(1, x) }) ?

      // potential pitfalls:
      sqrt(~.x**2 + ~.y**2) // equivalent to: sqrt(p => p.x**2 + p.y**2),
                            // instead use:   (~.x**2 + ~.y**2).sqrt()

      foo(bar(1, ~))        // equivalent to: foo(x => bar(1, x)),
                            // instead use:   ~.bar(1, ~).foo()

      foo(bar(~))           // invalid
                            // instead use:   ~.bar().foo()

      foo(~).prop           // invalid
                            // instead use:   ~.foo().prop

      #tag foo(~)           // invalid
                            // instead use:   ~.foo().#tag()

  This is similar to the [JS Partial Application proposal], the [magic Scala `_`
  underscore], or the [Tulip autovar]. I originally wanted to use `_` just like
  Scala (`_ + 1` reads so nicely as "blank plus one"), but then I realized that
  `_ is #tag _` (equivalent to `x => x is #tag _`) looks weird, since the `_`
  means two different things in two different places. `_` *has* to be the
  pattern-matching wildcard because it should match destructuring, and `_` is
  universally the destructuring wildcard.  
  TODO: open a ticket to bikeshed syntax  
  I briefly considered whether this could be type-dependent, like:

      // Consider:
      Let foo = (x, y) => x + y
      Let bar = x => 2*x
      Let qux = f => cmd: Do console_log(f(100))

      // That means:
      //   foo: (number, number) => number
      //   bar: (number) => number
      //   qux: ((number) => number) => VoidCmd

      // But when parsing:
      qux(bar(foo(1, ~)))

      // We get the equivalent of:
      qux(bar(x => foo(1, x)))

      // which is a type error (bar() takes in a number).

      // Theoretically we could notice that qux() takes in a unary function,
      // and instead parse it as:
      qux(x => bar(foo(1, x)))

      // But...that's crazy. Like, for starters, it's crazy to have parsing
      // dependent on whole-program type inference.
      // More concretely, if the type of `bar()` were to change drastically
      // to take in a function instead of a number (such as if `bar` were
      // shadowed by the name for some totally different function), that could
      // change the parsing, without this file having changed at all.
      // The correct error message that should result is that hey, you call
      // `bar()` and give it a number and that used to be correct, but now it
      // takes a function which is totally different.
      // But instead, because the parsing changed, the resulting error message
      // would potentially be that the return type of `bar()` doesn't match
      // the parameter type of `qux()`, even if neither of those changed at all.

[template literal syntax]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
[JS Partial Application proposal]: https://github.com/tc39/proposal-partial-application
[magic Scala `_` underscore]: https://medium.com/@LetsGoCard/yet-another-post-about-the-underscore-in-scala-ae7be854f0d3
[Tulip autovar]: https://github.com/tulip-lang/tulip/blob/master/doc/intro.md#lambdas

#### Incompatibilities

The expression syntax is based on JavaScript's, with a few deliberate
incompatibilities in edge cases that I think JavaScript syntax is confusing:
- Identifiers may only have single, interstitial underscores
    + Valid: `this_is_totally_valid`
    + Invalid: `_foo`, `foo__bar`, `foo_`, `$foo`  
      (TODO: bikeshed, maybe allow `foo__bar`?)
- Exponentiation isn't chainable
    + Should `2**3**2` be `(2**3)**2 = 64`, or `2**(3**2) = 512`? JS and Python
      both say `512`, but I think that's confusing because it's the other way
      around for other non-associative operators, e.g. `2/3/4 = (2/3)/4`.
      In Mechanical, `2**3**2` is a syntax error
    + (Just like JS, `-2**2` is also a syntax error. This actually differs from
       Python, where `-2**2 = -(2**2) = -4`, but that's confusing because it
       look like it could be `(-2)**2 = 4`)
- Comparisons are chainable
    + In JS, `3 > 2 > 1` is false, which is confusing. In Mechanical, not only
      does `a > b > c` behave as expected, but so does `a > b >= c == d`, or
      `a == b < c == d < e == f`. This feature is inspired by Python, however
      unlike Python, constructions like `a < b > c` are prohibited (they have
      to point the same way), and `!=` can't be chained at all.
    + (`!=` can't be chained because should `1 != 2 != 1` be true or false?)
- No [holes] in arrays, but trailing commas are allowed, and you can use the
  null value `#none`

      // valid:
      [ 1, 2 ]
      [ 1, 2, ]
      [
        1,
        2,
      ]
      [ 1, #none, 2 ]
      [ 1, 2, #none, ]

      // invalid:
      [ 1, , 2 ]
      [ 1, 2,, ]

- Mechanical records are much more restricted than JS objects. Field names must
  be valid identifiers, not arbitrary strings or numerals, and cannot be quoted.
  They also cannot be [computed], cannot be [method definitions], and there are
  no [getters or setters]. Trailing commas are allowed though!

      // valid
      { valid_identifier: 1 }

      // invalid
      {
        invalid__ident: 1,
        _invalid: 2,
        $invalid: 3,
        "not an identifier at all": 4,
        5: 6,
        [compute(7, 8)]: 9,
        method() {
          Return 10
        },
      }

- Only arrow function expressions (e.g. `x => 2*x`) are supported (there's
  no `function` keyword), functions must take at least one argument (all
  functions are pure, so what would a no-argument function do?), and trailing
  commas aren't allowed  
  (TODO: method-calling syntax/UFCS, named args when >2 params)

      // valid
      x => 2*x
      (x, y) => sqrt(x**2 + y**2)

      // invalid
      () => 1
      (x, y,) => sqrt(x**2 + y**2)
      function (x) { return 2*x }

- No bitwise operators
    + We have none of `~`, `&`, `|`, `<<`, `>>`, `>>>` built-in, but I hope to
      introduce a built-in macro
- JS-specific things that don't make sense with Mechanical semantics:
    + No "strict in/equality" `===`/`!==`. Regular in/equality `==`/`!=` is
      already strict
    + No increment/decrement operators `++`/`--`
    + No `in` or `instanceof` relations
    + No assignment operators `=`, `+=`, `-=`, `*=`, `/=`, etc
    + No comma operator

[holes]: https://2ality.com/2015/09/holes-arrays-es6.html
[computed]: https://2ality.com/2014/12/es6-oop.html#computed-property-keys
[method definition]: https://2ality.com/2014/12/es6-oop.html#method-definitions
[getters or setters]: https://2ality.com/2015/08/object-literals-es5.html#ecmascript-5-has-getters-and-setters

## Semantics

(TODO: the previous section and this section are a bit too much of a brain-dump,
 and need to be entirely reorganized for readability)

- there are no mutable data structures, instead mutable state variables may be
  changed from one immutable data value to another
- data values
    + the 3 scalars, booleans, numbers, and strings, are the same as in JS
        - (TODO: Unicode is messed up in JS strings because of UTF-16 heritage,
           do we want to fix them to be UTF-8?)
    + arrays are similar to JS. Arrays of a single type (e.g. `[1, 2, 3]` or
      `['one', 'two', 'three']`, aka monomorphic arrays) are exactly like in JS
      but immutable. An array with a mix of strings and numbers (`[1, 'two', 3]`)
      isn't allowed unless the values are hashtagged: `[#year 1, #name 'two', #year 3]`.
      When reading from the array, you can then pattern-match on the hashtags.
      It is recommended you choose descriptive hashtags. More on hashtags and
      pattern-matching below.
        - (TODO: we plan to eventually allow "dynamic-dispatch arrays" that allow
           mixing of types as long as they all support a given interface, like
           [Rust's dynamic dispatch trait objects];
           for consistency, shouldn't we allow you to do that with our extensible
           sum type in addition to interfaces/traits, too? The type system can
           actually remain totally sound, and we can still add friction by
           requiring an extra keyword like `mixed` in the array declaration.
           TODO: open a ticket about this)
    + records are like TypeScript interfaces, i.e. JS objects with fixed fields
      and types, but every field is required, no optional fields although fields
      can potentially have the null value `#none` (more on hashtags below).
      Records are structurally typed ([similar to TypeScript interfaces], except
      using [row polymorphism] like Elm). Field names aren't namespaced (unlike
      hashtags) so can't be private/opaque.
        - (Private/opaque record fields are compatible with the type system, is
           there any use case? I can't think of one, especially not one where an
           invisible field would make more sense than a visible field with an
           opaque hashtagged type. TODO: open a discussion ticket)
    + (hash)tagged unions are like [extensible sum types] (known in OCaml as
      [polymorphic variants]), except they're namespaced/scoped by file so you
      can create opaque types and newtypes with them.
        - I should write a blogpost on this, afaik this idea is totally unique
          to Mechanical. I think it's really cool because it provides the
          benefits of a [nominative] algebraic data type without name clashes,
          including [opaque types] and [newtype], while avoiding the need not
          only for type annotations but even for type declarations.
    + there are also global built-in `#none` and `#error _` types (`#error`
      takes a parameter), auto-imported into every module. Whereas other hashtag
      types can only be unioned with other (non-opaque) hashtag types, these two
      can be unioned with any type at all including primitives and opaque types.
      `T | #none` is the equivalent of other language's [Option or Maybe type],
      and `T | #error E` is the equivalent of other language's [Result or Either
      type].
        - Bonus: `T | #none | #error E` and `T | #error E | #none` are the same
          thing, whereas in e.g. Rust people have to decide [whether to use
          `Result<Option<T>, E>` or `Option<Result<T, E>>`], and there are even
          methods to [swap between them]
- function and command values
    + functions' and commands' return values have to be a single type
      (monomorphized), like arrays. Like arrays, if a function does sometimes
      return a string and other times a number, just use hashtags:

          // valid:
          (x) => (x > 100 ? #msg "a string" : #count x/100)

          // invalid:
          (x) => (x > 100 ? "a string": x/100)

    + other than that, pure functions are pure functions, shouldn't have
    + surprises coming from JS
    + commands are values that can be thought of two ways:
        - mathematically they're pure functions from state to new state and
          possibly a return value
        - or, in JS terms they're zero-argument side-effectful functions. That's
          actually how they're implemented in the first compilation pass, but
          **TODO** the immediately-invoked-zero-argument-function-value should
          be optimized away in later compiler passes
    + in Mechanical, all functions are pure functions, and cannot have
      side-effects. But they can return a command value, and you can `Do` a
      command to perform side-effects:

          When button.Click:
            Do foo(123)
            Let c = cmd:
              Do bar('thing')
              Do baz('another thing')
            Do c

          foo = (x) => cmd:
            Do qux(x + 1)

      As you can see, `When`-handlers can `Do` commands, and a `cmd:` block can
      define a new command in terms of `Do`-ing commands. That's it, there are
      no other ways to `Do` a command, but functions like `foo()` can return a
      `cmd:` block, allowing composition of side-effectful commands. Note that
      `foo()` is still a pure function, one that takes in a number and returns
      a command value.
      <details>
        <summary>
          (If this strikes you as basically like a Haskell monad, that's because
           it is. I think my way of describing it is easier to understand, at
           least coming from an imperative programming mindset, though.)
        </summary>

        - The obvious important difference from Haskell monads is that it's
          hardcoded to a specific monad that's like a combo IO and State monad.
          Instead of generalizing to other monads, we generalize by having all
          external side-effect functions be dependency-injected. This might
          sound unnatural, but it's actually a natural consequence of following
          capability-based security principles.
        - This model of dependency-injecting all external side-effect functions
          has the advantage of easily composing different side-effects without
          monad transformers and tediously "lifting" functions from one monad
          to another. This is kinda like algebraic effects in that way, however:
            + <details>
                <summary>What are algebraic effects?</summary>
                They're an evolution of monads. Monads are cool because they can
                represent side-effects and control flow in a way that remains
                referentially transparent, and remains statically type-checked,
                and in fact even side-effects are statically checked and
                constraints on side-effects statically enforced, by encoding
                side-effects into the type system. (Contrast with most statically
                typed languages like Java, C#, or OCaml where the type system
                is solely concerned with parameter types and return types and
                entirely ignores side-effects.) Algebraic effects do all of
                that too, but allow different side-effects to compose more
                easily than monads.

                - (Without side-effects, after all, Haskell is a ["completely
                  useless language—you have this black box, and you press Go,
                  and it gets hot, but there's no output!"][Haskell is useless])
                - If you're coming from a Haskell background, the first few
                  pages of [the Edwin Brady paper] presents an especially clear
                  motivation for the problems with monad transformers and how
                  algebraic effects can help.
                    + The paper uses Brady's language Idris, which is like
                      Haskell but with dependent types. There are also plain
                      Haskell implementations, like Oleg Kiselyov's
                      [extensible-effects/free-er monad] which instead of
                      dependent types uses extensible sum types much like
                      Mechanical's hashtagged unions (the paper calls them
                      "open unions").
                - From a JavaScript, OCaml, or Elm background, I recommend
                  reading [the Koka overview] Sections 2.1-2.9, and then checking
                  out [Section 1.4][Koka primer on effect handlers]. (At least for
                  Koka v1; Koka v2 is in development, please let me know if you
                  notice the book has changed too.)
                - If your primary exposure to algebraic effects is Dan Abramov's
                  Overreacted blogpost [Algebraic Effects for the Rest of Us],
                  then I *especially* recomend reading the Koka overview.
                  Dan Abramov, coming from a React-focused, dynamically typed,
                  imperative perspective, is focused on how effect handling
                  impacts control flow (treating effects _algebraically_ is
                  actually irrelevant to his discussion).  
                  Mechanical, a statically typed language with no conventional
                  control flow, instead draws inspiration from efforts in purely
                  functional languages (which also have no conventional control
                  flow) to use algebraic effects as a substitute for control
                  flow, while remaining referentially transparent and statically
                  (and algebraically) typed.
                - If your primary exposure to algebraic effects is Multicore
                  OCaml then I also recommend reading the Koka overview.
                  Multicore OCaml is also focused on how effect handling impacts
                  control flow, because Multicore OCaml is impure. Mechanical
                  has no conventional control flow and draws inspiration from
                  efforts in purely functional languages (which also don't have
                  control flow) to use algebraic effects as a substitute for
                  control flow, while remaining referentially transparent.
                - A lot more literature—way more than I could read—available at:
                  https://github.com/yallop/effects-bibliography
              </details>
            + Algebraic effects introduce elements of imperative programming
              that I greatly dislike: control flow and pervasive implicit
              ordering.
                - In imperative languages, and even impure functional languages
                  like OCaml or Lisp, [sequence points] are defined in virtually
                  every language construct specifying that, for example, in
                  `foo(bar(), baz())`, the order of evaluation is implicitly
                  `bar()`, then `baz()`, then `foo()`. In purely functional
                  languages and dataflow languages, order of evaluation doesn't
                  matter, so you don't even have to think about that—except that
                  the order of side-effects can matter, so algebraic effects
                  impose a total ordering on effects by [nesting continuation
                  functions].
                - In my opinion this is throwing the baby out with the bathwater:
                  just because the order of *some* side-effects *can* matter,
                  doesn't mean we need to impose a total ordering on *all*
                  side-effects, regardless of whether the order *does* matter.
                  (This is a generalization of the implicit ordering of monads
                   like Haskell's `IO` monad; by contrast, in Mechanical all
                   side-effects are concurrent unless explicitly ordered, see
                   below.)
            + Algebraic effects are also more powerful than necessary, letting
              you do parlor tricks like evaluating one computation along
              multiple branches by feeding it multiple possible return values
              for an effect. I can't think of much practical use for this power,
              but I can see the practical downsides it has for performance
              (the continuation must be allocated or copied to the heap, rather
               than just temporarily on the stack).
                - Multicore OCaml limits continuations to one-use-only for that
                  reason (performance). Their version of algebraic effects are
                  basically just typechecked coroutines, their design documents
                  say their purpose is to allow userland scheduling of fibers,
                  which is a very different purpose from what Mechanical would
                  consider, which is an alternative to monads for modeling
                  side-effects.
                - An example of such a parlor trick is the "ambiguous" effect
                  handler from the [Koka primer on effect handlers], which
                  evaluates a computation for every possible return value of
                  an effect "simultaneously" (so if the computation flips a
                  coin twice and returns a tuple of the two results, then
                  evaluating the computation with this handler returns
                  `[(Heads, Heads), (Heads, Tails), (Tails, Heads), (Tails, Tails)]`
                  or similar).
                - In fact, Koka is an informative contrast with Mechanical.
                  Hardcoding a monad with exactly 2 effects (state mutation
                  and foreign function calls) makes Mechanical isomorphic to
                  a subset of JavaScript, which makes compilation very easy.
                  By contrast, the Koka compiler represents such fully general
                  effect handlers using CPS ([continuation-passing style]).
                  Naive compilation of CPS to JavaScript would cause every
                  stack frame to be allocated as a closure on the heap, so
                  Koka uses a sophisticated [type-directed transform] to
                  recover the information that was "lost" by generalizing to
                  fully general effects, but that Mechanical has hardcoded.  
                  In theory, that should mean that Koka code that is equivalent
                  to Mechanical's hardcoded effects compiles to equally
                  performant JS, and only effects that *needs* to allocate
                  continuations on the heap, like the "ambiguous" effect,
                  will pay that performance penalty. (Whereas Mechanical's
                  hardcoded monad can't represent such effects at all.)  
                  In principle, there's nothing stopping Mechanical from
                  adopting the same approach. But it makes the compiler
                  more complex and slower, so I'm still waiting to learn
                  of a practical use case before I consider it.
        - The other important difference from Haskell monads is ordering. If
          you have a sequence of `IO` actions in a `do`-block, the resulting
          side-effects are guaranteed to be performed in that order.  
          By contrast, Mechanical is a concurrent language. `Do` statements
          perform side-effects in no particular order unless one is specified,
          and in the future I'm hoping the compiler can auto-parallelize
          programs among Web Workers or even pthreads, although that's very
          far in the future.  
          (The 2 ways to explicitly specify order being `After Got` statements
           and witness variables, see below.)
      </details>
    + there are 4 types of command values: pure, sync, async, and void
        - void commands are commands with no `Return` statement and therefore
          don't return anything. They can `Do` any other type of command.
        - asynchronous commands either have their `Return` statement after an
          `After Got` statement, or return the result of `Do`-ing an async
          command. They can `Do` any other type of command.

              // examples:
              cmd:
                Future x = Do async_thing
                ~ After Got x ~
                Return 2*x

              cmd:
                Do something
                Return Do async_thing

              cmd: Do async_thing

        - synchronous commands cannot have `After Got` statements, and can
          only `Do` an async command if they ignore its return value.

              // examples:
              cmd:
                Get x = Do sync_thing
                Change state.x to x
                Return x + 1

              cmd:
                Do async_thing
                Do sync_thing
                Return Do another_sync_thing

              cmd: Do sync_thing

        - pure commands have no side-effects, can only `Do` other pure commands,
          and exist primarily for testing. The built-in function `eval()` can
          evaluate them and return the resulting value
           + examples: `cmd: 0`, `cmd: 1+1`, `cmd: Do some_pure_cmd`,

                 cmd:
                   Let x = 1+1
                   Return x + 3

           + (no relation JavaScript's `eval()`. Coincidentally, `eval()` is
              also special, but in a very different way: it's entirely erased
              by the compiler, because a zero-argument pure function serves
              no purpose at runtime)
               - **TODO:** should/can we unwrap pure commands without `eval()`,
                 so that `cmd: 0` and `0` are exactly equivalent?
        - non-void commands (pure, sync, async) also have a return type and are
          [covariant] in their return type, so a pure command returning `#cat`
          is a subtype of a pure command returning `#cat | #dog`. They also form
          a hierarchy: `PureCmd<#cat>` is assignable to `SyncCmd<#cat>` which is
          assignable to `AsyncCmd<#cat>` which is assignable to `VoidCmd`
    + TODO: in Elm and Haskell, functions cannot be compared for equality.
      In Mechanical, we think it's nice to provide function equality so they can
      be used in Maps and such. Currently, we guarantee three useful invariants
      about function equality:
        - Firstly, we guarantee that it's an [equivalence relation]:

              No matter what, x == x             (reflexivity)
              If x == y, then y == x             (symmetry)
              If x == y and y == z, then x == z  (transitivity)

        - Secondly, we guarantee functional [referential transparency]:

              If x == y, then f(x) == f(y)

          Note that this isn't true in JavaScript:

              let f = c => (x => x + c);
              f(1) === f(1) // false

              // whereas in Mechanical:

              Let f = c => (x => x + c)
              f(1) == f(1) // true

          Importantly, this applies **_only_** to function calls, not other
          expressions:

              Let g = x => x + 1
              Let h = x => x + 1
              Let k = ~ + 1

              Does g == h or g == k? No guarantees either way

              Does g == f(1)? Currently, no guarantees either way. In the
                              future, this may be guaranteed to be false.

        - However, for some literal functions it's useful to guarantee equality
          (the third guarantee):

              ~.prop == ~.prop
              #tag ~ == #tag ~
              #tag ~ == ~.#tag()

              // file1.mech
              Export a = ~.prop
              Export b = #tag ~

              // file2.mech
              From 'file1.mech' Import a, b
              a == ~.prop   // true
              b == #tag ~   // true
              b == ~.#tag() // true

          But for anything else, even just arrow functions, all bets are off:

              Does ~.prop.another == ~.prop.another ? No guarantees
              Does ~.prop == (x => x.prop) ?          No guarantees
              Does #tag ~ == (x => #tag x) ?          No guarantees

          TODO: this is currently one of the few instances of undefined behavior
          in the language, should we specify it? I don't see any way to specify
          it that will be forward-compatible with the compiler unifying functions
          with fancier AST-equivalence heuristics.
- operators
    + TODO
- pseudo-expressions: `Do <expr>`, `Current <state>`, `Initial <state>`
    + unlike other expressions, these aren't referentially transparent: a given
      pseudo-expression may evaluate to different things at different times,
      and may even perform side-efffects. Therefore they're really part of an
      imperative statement, see below
- statements
    + Mechanical has **no conventional control flow**
        - the mental model for a Mechanical program is a state machine.
          State machines don't have control flow, they atomically transition
          from one state to another in response to an event.
          The new state, and the set of side-effects triggered, is a pure
          function of the previous state and the event.
        - the set of side-effects is, conceptually, unordered, since the
          transition is atomic. Therefore, Mechanical has to be a concurrent
          language, which is to say, all imperative statements "run"
          concurrently:

              // in theory, these may print in any order:
              Do console_log('hello 1')
              Do console_log('hello 2')

              // these sequences happen concurrently:
              Do cmd:
                Do console_log('foo 1')
                Future wait = Do timeout(1000)
                ~ After Got wait ~
                Do console_log('foo 2')

              Do cmd:
                Do console_log('bar 1')
                Future wait = Do timeout(1000)
                ~ After Got wait ~
                Do console_log('bar 2')

              // in theory, that could print any of:
              foo 1, bar 1, foo 2, bar 2
              foo 1, bar 1, bar 2, foo 2
              bar 1, foo 1, foo 2, bar 2
              bar 1, foo 1, bar 2, foo 2

            + except for 2 ways to explicitly specify order:
                - `After Got` statements (see below)
                - witness variables:

                      Get x = Do foo()
                      Get y = Do bar(x)

                  The `bar(x)` command of course has to happen after `foo()`,
                  because it uses `x`, which comes from doing the `foo()`
                  command.
        - `Return` statements are only allowed in "tail position",
          "early returns" aren't allowed. In theory the position shouldn't
          matter but it helps avoid confusion:

              // valid:
              cmd:
                Do some_cmd
                Return Do some_other_cmd

              cmd:
                If x > 0:
                  Return Do some_cmd(x)
                Else:
                  Do some_other_cmd
                  Return #error "x must be positive"

              cmd:
                Match x:
                  #name str ->
                    Return str
                  #year num ->
                    Return num.as_string()

              // invalid:
              cmd:
                Return Do some_cmd
                Do some_other_cmd
                // ^ some_other_cmd *looks* like it should be skipped by the
                //   "early return", but under Mechanical semantics they both
                //   run concurrently, which is confusing

              cmd:
                If x > 0:
                  Return Do some_cmd(x)

                Do some_other_cmd
                Return #error "x must be positive"

        - There are no looping statements. Instead, use higher-order functions
          like map and reduce:

              // parallel HTTP requests
              Future pages = urls.map(url => Do fetch(url))

              // sequential HTTP requests
              Future pages = urls.reduce(
                from: [],
                update: (pages_so_far, next_url) => (Do cmd:
                  ~ After Got pages_so_far ~
                  Future next_page = Do fetch(next_url)
                  ~ After Got next_page ~
                  Return [...pages_so_far, next_page]
                )
              )

          Compare with equivalent JavaScript:

            ```js
            // parallel HTTP requests
            const pages = Promise.all(urls.map(url => fetch(url)));

            // sequential HTTP requests
            const pages = urls.reduce(update, []);
            async function update(pagesSoFar, nextUrl) {
              pagesSoFar = await pagesSoFar;
              const nextPage = await fetch(nextUrl);
              return [...pagesSoFar, nextPage];
            }

            // or in ES5:
            var pages = urls.reduce(update, Promise.resolve([]));
            function update(pagesSoFar, nextUrl) {
              return pagesSoFar.then(pagesSoFar => {
                return fetch(nextUrl).then(nextPage => {
                  return [...pagesSoFar, nextPage];
                });
              });
            }
            ```
          (TODO: consider making this easier with a for-loop construct
           based on for-await-of semantics)

    + the only compound statements are 2 branching statements:
        - `If` statements:

              If x > 0:
                Do some_cmd(1)
              Else If x < 0:
                Do some_cmd(-1)
              Else:
                Do some_cmd(0)

          The `Else If` and `Else` clauses are optional. The conditional must
          be an explicit boolean expression, there is no coercion to boolean,
          no truthy and falsy values.
        - `Match` statements, for pattern-matching:

              // you usually pattern-match on tagged values:
              Match x:
                #const_tag ->
                  // some tags have no parameter
                  Pass
                #tag val ->
                  Do some_cmd_using(val)
                #point { x, y, z } ->
                  // can destructure records
                  Do some_cmd_using(x**2 + y**2 + z**2)
                #tag1 | #tag2 ->
                  // can match multiple tags
                  Pass
                #year num | #date { year: num } ->
                  // when destructuring multiple tags, variables must have
                  // consistent types
                  Do some_other_cmd_using(num)
                #tagA var | #tagB ->
                  // or if a variable isn't in every pattern, it may be #none
                  If var == #none:
                    // x must be #tagB
                    Pass
                  Else:
                    // x must be #tagA
                    Pass
                #another_tag _ ->
                  // use _ to ignore a value when destructuring
                  Pass
                _ ->
                  // wildcard: match anything
                  Pass

              // you can also pattern-match on records:
              Match y:
                { prop1: #tag1, prop2: val } ->
                  Do some_cmd_using(val)
                { prop1: #tag2, prop2: { thing: val } } ->
                  Do some_cmd_using(val)

          (TODO: I haven't decided if patterns should be required to be
           mutually exclusive (except for the wildcard pattern, ofc))
    + in a non-`cmd:` arrow function, the only leaf statements allowed are
      pure `Let` statements and `Return` statements:

          foo = x =>
            Let y = foo(x)
            Return bar(y)

    + in a `cmd:` block or `When`-handler, there are 4 kinds of imperative
      leaf statements:
        - plain `Do` statements, which don't expect a return value and can
          `Do` any type of command including void commands:

              Do any_command

        - `Get…Do` statements, for synchronous commands:

              Get y = Do sync_cmd(x)
              Get y = pure_fn(Do sync_cmd(x))
              Get y = sync_cmd(x).Do!.pure_fn()

          and for getting the `Current` value of state variables (or `Initial`,
          during initialization):

              Get now = Current system.unix_time
              Get age = age_from_birthday(Current birthday_field.value)

        - `Future…Do` statements and `After Got` statements, for async
          commands:

              Future page = Do fetch(url)

              // At this point, you can't use `page`, because it doesn't have
              // the result of the `fetch()` yet, it has a promise that will
              // eventually resolve to the result of the `fetch()`.
              // In Mechanical, promises aren't first-class values, the only
              // thing you can do with a promise is "await" it with an
              // After-Got-statement (the ~'s (tildes) are optional):

              ~ After Got page ~

              // Now `page` is the result of the `fetch()`, and we can use it:
              Change help_pane.contents to page.body

              // Note that at this point, we're in the future. Any number of
              // events might have happened, handlers might have run, state
              // might have changed, side-effects might have been performed
              // since before the `~ After got page ~` statement.
              //
              // For comparison, consider JavaScript's `await` operator:
              //
              //   foo(bar(), await baz(), qux())
              //
              // In this expression, `bar()` and `baz()` (which are on either
              // side of the `await` keyword) happen immediately, whereas `foo()`
              // and `qux()` (also on either side of the `await` keyword) happen
              // in the future, after the promise resolves.
              //
              // By contrast, in Mechanical, it's clearer and more explicit
              // what happens immediately, vs what happens `After` the promise
              // resolves.
              //
              // (Callbacks would also be more explicit, but `After Got` is more
              //  convenient and easier to read than `.then(page => { ... })`
              //  callbacks or worse, nested callback hell.)


              // Another point of comparison, consider:
              //
              //    const thing1 = await fetch(thing1_url);
              //    const thing2 = await fetch(thing2_url);
              //    const thing3 = await fetch(thing3_url);
              //
              //    make_use_of(thing1, thing2, thing3);
              //
              // This is a very natural way to use `await`, but is also a
              // well-known anti-pattern!
              // It makes the 3 HTTP requests sequentially, when in most cases
              // it would be better to parallelize them:
              //
              //    let thing1 = fetch(thing1_url);
              //    let thing2 = fetch(thing2_url);
              //    let thing3 = fetch(thing3_url);
              //
              //    [thing1, thing2, thing3] =
              //      await Promise.all([thing1, thing2, thing3]);
              //
              //    make_use_of(thing1, thing2, thing3);
              //
              // By contrast, in Mechanical, the better, parallel way is also
              // the more natural way to use `After Got`:

              Future thing1 = Do fetch(thing1_url)
              Future thing2 = Do fetch(thing2_url)
              Future thing3 = Do fetch(thing3_url)

              ~ After Got thing1, thing2, thing3 ~

              Do make_use_of(thing1, thing2, thing3)

              // If you *deliberately* want to sequentialize them, you can:

              Future thing1 = Do fetch(thing1_url)
              ~ After Got thing1 ~
              Future thing2 = Do fetch(thing2_url)
              ~ After Got thing2 ~
              Future thing3 = Do fetch(thing3_url)
              ~ After Got thing3 ~
              Do make_use_of(thing1, thing2, thing3)

              // TODO: also explain why promises aren't first-class values
              // by explaining how Future pages = urls.map(Do fetch(~)) works
              // and showing how that's more convenient than JS

          (TODO: should we have a `~ After Got Future y = Do x ~` statement,
           for convenience?)

        - `Change…To/By` statements, which update state:

              Change name To "Chuck Finley"

              // the following are equivalent:
              Change counter To (Current counter) + 1
              Change counter By c => c + 1
              Change counter By ~ + 1

          It's an error to update the same state multiple times in the same
          event handler

              // ERROR: `counter` changed by multiple commands
              Change counter By ~ + 1
              Change counter By ~ + 1

              // valid:
              Change counter By ~ + 1
              Future x = Do foo
              ~ After Got x ~
              Change counter By ~ + 1

          This is even tracked for first-class command values:

              Let inc_counter = cmd:
                Change counter By ~ + 1

              When Event:
                Do inc_counter
                Change counter By ~ + 1
                // ^ ERROR: `counter` changed by multiple commands

          (This is tracked by the type system, pretty conservatively, with no
           escape hatch. TODO: is there any use case for an escape hatch?)  
          You can update separate properties of the same record separately
          though, even nested:

              Change player.loc.x By 10 * ~
              Change player.loc.y By 5 * ~

          You can also update separate items of the same array, with caveats:

              // valid:
              Change an_array[0] To "thing"
              Change an_array[1] To "another thing"

              Change other_array[i] To "thing"
              Change other_array[i+1] To "another thing"

              // invalid:
              Change an_array[0] To "thing"
              Change an_array[0] To "another thing"

              Change other_array[5] To "thing"
              Change other_array To ["another thing", ...Current an_array]

              // compiler can't tell if valid, requires annotation:
              Change an_array[0] To "thing"  ^assert_no_conflict
              Change an_array[i] To "another thing"  ^assert_no_conflict

              Change an_array[i] To "thing"  ^assert_no_conflict
              Change an_array[j] To "another thing"  ^assert_no_conflict

              // the ^assert_no_conflict annotation is only allowed for
              // multiple commands in the same `When` handler.
              // Different `When` handlers for the same event aren't
              // allowed to update possibly-conflicting items even with
              // the annotation, as a matter of style.
              // TODO: should we allow it? Let's open a discussion ticket

    + the only other statement is `Pass`, which is both a no-op statement and
      a no-op command value (TODO: bikeshed name)
    + and that's it! No more statements. No while/for-loops, no break/continue,
      no throw statement, because remember, Mechanical doesn't have control
      flow
- declarations
    + whereas statements are allowed in `cmd:`-blocks and `When`-handlers,
      declarations are only allowed at the top-level
    + there are 2 imperative statements allowed at the top-level, plain `Do`
      and `Get…Do` statements, to perform side-effects during initialization
        - to use initial values of state, use `Initial`, not `Current`
        - `Future…Do` and `After Got` aren't allowed at top-level because
          we don't want to block initialization, but you can `Do` a `cmd:`
          which can have anything a `cmd:` can have:

              Do cmd:
                Future y = Do foo(x)
                ~ After Got y ~
                Do stuff_with(y)

        - `Change` statements aren't allowed at top-level because state should
          just be initialized to the right value in the first place
        - `Let` is redundant with just defining regular top-level constants
        - `Return` obviously can't be allowed outside of a function or command
        - unless top-level `Pass` is useful for macros or something, there
          seems no reason to allow it
    + the 2 branching statements (`If` and `Match`) are also allowed at the
      top-level, and inside them, `Let` and `Pass` are allowed
    + state is the beating heart of Mechanical's state-machine-based
      conceptual model of programming. Consider:

          // irreducible mutable state:
          State seconds = 0

          // computed state:
          minutes = floor(seconds/60)

          When ClockTick:
            Change seconds By ~ + 1
            Do console_log(
              `minutes: ${Current minutes}, seconds: ${Current seconds}`)

        - irreducible, [essential] mutable state like `seconds` is declared
          using `State`
        - computed state like `minutes` is defined as a pure expression of
          other state. It cannot be directly mutated, but whenever `seconds`
          is mutated, Mechanical will also update `minutes` to match.
          <details>
            <summary>The amazing thing is that Mechanical updates computed
            state with efficient mutations where possible, rather than
            recomputing it from scratch.</summary>

            For example:

              State counters = [1, 2, 3]
              doubled = counters.map(2 * ~)

              increment = i => cmd:
                Change counters[i] By ~ + 1

            Compiles to:

              ```js
              var counters = [1, 2, 3];
              var doubled = counters.map(x => 2*x)

              increment = i => {
                counters[i] = counters[i] + 1;
                doubled[i] = 2 * (doubled[i] + 1)
              };
              ```

            Note that this is only possible because Mechanical is purely
            functional and statically typed.  
            (What are the limitations of this? Well, it should work as long
             as you don't use a custom loop/recursive function on a list.
             So using built-in map, filter, reduce, sort, slice, etc should
             all be similarly transformable, but if you wrote your own sort
             function, for example, the Mechanical compiler would give up and
             re-run your custom sort whenever the upstream state updated.
             You can still provide a custom mutative update function even in
             that case, though, and Mechanical still helps you out, see the
             `Derive State…From` declaration below).
          </details>

        - event handlers are declared using `When`. They take a stream of
          events and optionally a parameter name or destructuring expression,
          and then a indent block of statements to run when the event happens.
          Statements that update state are state transitions of the state
          machine. Statements that perform side-effects are side-effects of a
          state transition (possibly a transition to the same state).  
          Operationally, any number of `When` handlers for the same event
          stream are fine as long as they don't update conflicting state
          (or even annotated possibly-conflicting state, for now, see section
          on the `Change` statement above).  
          As a matter of style, multiple `When` handlers in the same module
          for the exact same event stream aren't allowed, although different
          modules can share an event stream and each define `When` handlers
          on it. Note that an event can still trigger multiple handlers in the
          same module if included in multiple stream expressions, for example
          if streams `A`, `B`, and `C` each have their own event handler and
          there's also a handler for `any([A, B, C])`.  
          (TODO: maybe it should be allowed with an annotation? Let's open a
           discussion ticket)
        - Advanced Feature: there's also a special `When…Changes` handler:

              When minutes Changes To new_minutes:
                Do redraw_minute_hand(new_minutes % 60)

              When floor(60*minutes) Changes To new_hour:
                Do redraw_hour_number(new_hour % 12)

          This is meant for building abstractions on state: the code that
          updates `seconds` doesn't have to worry about redrawing stuff at
          all, and can pretend that the view is a pure function of state
          (which it is! Conceptually. Actually updating the DOM is necessarily
          imperative, though). Note that this allows the view layer to be
          implemented entirely in userland (contrast with Elm where the view
          layer can only be implemented as part of the language runtime).  
          `When…Changes` runs when a state expression changes, and it can only
          perform side-effects, it cannot update state. To update state
          when a state expression changes, use `Derive State…From`, below.
        - Advanced Feature: while computed state is derived from other state
          via a pure expression, you can also create state derived from other
          state via mutative updates. There are two versions of this:
            + (TODO) If computed state is *conceptually* a pure function of
              other state, but Mechanical is unable to automatically generate
              efficient mutative updates, you can do so manually:

                  // custom reverse function, for some reason
                  reverse = match ~:
                    []               -> []
                    [first, ...rest] -> [...rev(rest), first]


                  State counters = [1, 2, 3]
                  Derive State reversed From counters:
                    Pure: reverse(counters)
                    Update(counters: ops):
                      // for arrays, the update step gets a diff representing
                      // the mutative updates to the array. The diff itself
                      // is an array of actions a "cursor" performed on the
                      // array:
                      //   #skip number   (number of current items to skip)
                      //   #remove number (number of current items to remove)
                      //   #insert items  (array of new items to insert)
                      //   #replace items (array of new items with which to
                      //                   replace current items)
                      //
                      // For example, if the number 42 was inserted at index 3
                      // (0-indexed), then Update would be given the diff
                      // [#skip 3, #insert 42].
                      ops.reduce(
                        from: {i: 0, result: Current reversed},
                        update: ((so_far: {i, result}, next: op) =>
                          match op:
                            #skip n ->
                              {i: i+n, result}
                            #remove n ->
                              {i, result: result.slice(
                                result.length()-i-n, result.length()-i)}
                            #insert items ->
                              {
                                i: i+items.length(),
                                result: result.splice(
                                  result.length()-i, 0, items),
                              }
                            #replace items ->
                              {
                                i: i+items.length(),
                                result: result.splice(
                                  result.length()-i,
                                  items.length(),
                                  items),
                              }
                        ),
                      )

              Yeah, it's kind of a pain, but you know what's amazing?
              You don't have to test it! Mechanical uses [property-based
              testing] to automatically generate possible values of
              `counters` and possible diffs, and ensures that your `Update`
              expression always results in the same thing that your `Pure`
              expression says the result *should* be.
            + If computed state is not even conceptually a pure function of
              other state, but rather the result of imperative commands,
              you can do that, too. The view layer is the canonical example:
              the set of event streams from the DOM is state, it changes
              when you add elements to the DOM, which happens in response
              to application state changing.  
              In other words, this is like a `When…Changes` handler except it
              can also update a single piece of state (which may be composed
              of multiple pieces of state, like a record).

                  State counters = [1, 2, 3]
                  Derive State view From counters:
                    Initial: counters.map(Do make_li_elements(~))
                    Update(counters: ops):
                      // blah blah blah (TODO)

    + modules are how code is organized. Every `.mech` file is automatically
      a module. Modules can be declared internal to a file with a `Module`
      declaration, which is useful for namespacing and encapsulation.
      A directory can also be a module if it has a `_module_.mech` file,
      although unlike `__init__.py` it's useless if it's empty because
      Mechanical modules are encapsulated, so `_module_.mech` has to export
      something to be useful.  
      Note that modules form a tree, unlike in Node or Python where modules
      are leaves and can't nest.
        - `Export` declarations are like JavaScript without the braces, but
          there's no `export default`, and the order has been switched from
          `export…from…` to `From…Export…`.  
          TODO: should we allow `From…Export *`? It doesn't actually create
          any names in the current namespace.
        - `Import` declarations have slightly different syntax from JavaScript:

              // switched around from JavaScript, and no braces {}
              From './lib.mech' Import x, y As renamed_y, z

              // to give a name to the module, use `From…As…` instead
              // of `* as <name>`
              From './lib mech' As library Import x, y, z

              // or just import the module by itself
              Import './lib.mech' As library

          Circular imports are allowed and work. There is currently no dynamic
          import of `.mech` files.
        - Mechanical also allows you to import values from JavaScript modules,
          but this works differently from FFI in other languages. In most
          languages, a module you import could import another module, which
          imports another module, which imports yet another module that you
          don't know anything about and have no reason to trust, and all of
          these transitive dependencies can run any code and do anything.  
          (Targeted attacks by transitive dependencies have occurred in the
          real world, e.g. [`event-stream`], [`electron-native-notify`],
          but you should be more worried about widespread accidental
          vulnerabilities, e.g. [ZipSlip] which affected dozens of libraries
          and projects across every major language, or similar attacks like
          [XXE], all instances of the more general [confused deputy problem].)  
          By contrast, Mechanical's design encourages [capability-based
          security], most importantly, the Principle of Least Authority.
          The key insight is that the only way for a Mechanical program to
          do anything dangerous, or indeed have any effect on the world
          (whether DOM, filesystem, network requests, etc), is to call out
          to JavaScript (or use a stdlib command that calls out to JavaScript,
          like the View layer).  
          So by design, Mechanical code has no intrinsic authority to affect
          the outside world. To have any effect on the world, the code must
          be given fine-grained authority to do so, preferably the minimum
          necessary to accomplish its purpose. Concretely:
            + in many cases you won't even notice this restriction, because
              many Mechanical command functions that wrap JS functions take
              a "foreign entity reference" as an argument (e.g. an HTML element,
              `File` object, etc), and those can be imported and called without
              restriction:

                  // wrapper.mech
                  From './lib.js' Import:
                    foo :: (*HTMLElement, string) => SyncCmd<*HTMLElement>
                    bar :: (*HTMLElement, attr: string, value: string) => VoidCmd

                  // untrusted.mech
                  From './lib.mech' Import foo, bar

                  Export qux = el => cmd:
                    Get el2 = Do el.foo('nested_child')
                    Do el2.bar(attr: 'title', value: 'Click me!')
                    Return el2

              Those are unrestricted because you have to pass an HTML element
              to them in order to use them, and that's fine-grained authority.  
              When calling out to JS functions, supported types for interchange
              are basically JSON (but immutable) along with undefined,
              functions, and opaque foreign entity references. `null` and
              `undefined` map to `#none`. Mutable objects and [host objects]
              are foreign entities, so to Mechanical code they're opaque
              references. JS functions map to Mechanical functions that return
              commands, except JS functions with no arguments map to just a
              command value.
                - The main inconvenience here is that this prevents libraries
                  from providing convenience functions that e.g. take in a
                  filename string instead of a `File` object. But if this
                  library is just supposed to act on one file, why are you
                  giving it access to your entire filesystem? What if it's just
                  supposed to parse YAML in a file, but it has a bug where
                  malicious content in the file can [execute arbitrary code]?  
                  A core tenet of [capability-based security] is "don't separate
                  designation from authority". In this example, the filename
                  string designates a file, but comes with no authority to
                  access the file, and instead, you implicitly authorize all
                  code in every library you call (and every library they call,
                  and so on) to access the entire filesystem, and much much
                  more. By contrast, a `File` object simultaneously designates
                  a file and authorizes access to it, which is fine-grained
                  Least Authority.
            + So how do you get a foreign entity reference ("ref" for short)
              in the first place? Usually, the JS library has at least one
              function whose arguments are Mechanical values, but whose return
              value includes one or more refs. Unlike the above, these cannot
              be `Import`ed. Instead, they can only be `Bootstrap`ed by the
              entry point module (the one you call `mechc` on), which has
              otherwise the same syntax as `Import`:
            + to call code that creates foreign entity references from nothing,
              must be passed in via `Init Params`
            + Init
            + to bootstrap, `Bootstrap`

          - The exception is the entry point module, which can d
          which requires type declarations:

              From './lib.js' Import:
                foo :: (*Element, string) => Command<*Element>
                bar :: (string, thing: number, another: string) => string
                baz :: string

          Type syntax is similar to TypeScript but instead of parameter names,
          you provide label names, and only for parameters that need labels
          (so, for functions with >=3 params, labels for 2nd param and up).  
          However,
    + Import/Export
        - Bootstrap, Init
        - Declare
    + %View
    + Module

[Rust's dynamic dispatch trait objects]: https://doc.rust-lang.org/book/ch17-02-trait-objects.html
[similar to TypeScript interfaces]: https://www.typescriptlang.org/docs/handbook/interfaces.html
[row polymorphism]: https://brianmckenna.org/blog/row_polymorphism_isnt_subtyping
[extensible sum types]: https://functor.tokyo/blog/2019-07-11-announcing-world-peace
[polymorphic variants]: https://dev.realworldocaml.org/variants.html
[nominative]: https://en.wikipedia.org/wiki/Nominal_type_system
[opaque types]: https://medium.com/@ckoster22/advanced-types-in-elm-opaque-types-ec5ec3b84ed2
[newtype pattern]: https://github.com/rust-unofficial/patterns/blob/master/patterns/newtype.md
[Option or Maybe type]: https://en.wikipedia.org/wiki/Option_type
[Result or Either type]: https://en.wikipedia.org/wiki/Result_type
[whether to use `Result<Option<T>, E>` or `Option<Result<T, E>>`]: https://www.reddit.com/r/rust/comments/5xcj2e/choose_resultoption_vs_optionresult/
[swap between them]: https://doc.rust-lang.org/std/result/enum.Result.html#method.transpose

[sequence points]: https://en.wikipedia.org/wiki/Sequence_points
[nesting continuation functions]: https://en.wikibooks.org/wiki/Haskell/do_notation#Translating_the_bind_operator
[Koka primer on effect handlers]: https://koka-lang.github.io/koka/doc/kokaspec.html#sec-a-primer-on-effect-handlers
[Haskell is useless]: https://www.youtube.com/watch?v=iSmkqocn0oQ&t=197
[the Edwin Brady paper]: https://eb.host.cs.st-andrews.ac.uk/drafts/effects.pdf
[extensible-effects/free-er monad]: http://okmij.org/ftp/Haskell/extensible/more.pdf
[the Koka overview]: https://koka-lang.github.io/koka/doc/kokaspec.html#sec-an-overview-of-koka
[Algebraic Effects for the Rest of Us]: https://overreacted.io/algebraic-effects-for-the-rest-of-us/
[continuation-passing style]: https://en.wikipedia.org/wiki/Continuation-passing_style
[type-directed]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/12/algeff.pdf

[covariant]: https://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science)
[equivalence relation]: https://en.wikipedia.org/wiki/Equivalence_relation
[referential transparency]: https://www.sitepoint.com/what-is-referential-transparency/
[essential]: https://bernhardwenzel.com/articles/out-of-the-tar-pit/
[property-based testing]: https://medium.com/criteo-labs/introduction-to-property-based-testing-f5236229d237

[`event-stream`]: https://medium.com/agoric/pola-would-have-prevented-the-event-stream-incident-45653ecbda99
[`electron-native-notify`]: https://blog.npmjs.org/post/185397814280/plot-to-steal-cryptocurrency-foiled-by-the-npm.html
[ZipSlip]: https://github.com/snyk/zip-slip-vulnerability
[XXE]: https://levelup.gitconnected.com/using-word-documents-or-docx-files-to-gain-unauthorised-access-to-server-private-resources-2477b3eafb7b
[confused deputy problem]: https://en.wikipedia.org/wiki/Confused_deputy_problem
[capability-based security]: http://habitatchronicles.com/2017/05/what-are-capabilities/
[host objects]: https://stackoverflow.com/a/7614380
[execute arbitrary code]: https://justi.cz/security/2017/10/07/rubygems-org-rce.html

## Comparisons

#### Elm
[Like Elm], Mechanical is statically typed, purely functional, and
side-effects are referentially transparent because they're represented
by values called "commands". Unlike Elm, Mechanical uses JavaScript-like
rather than Haskell-like syntax, including convenient imperative-like
syntax for commands.

Also, commands can be synchronous, allowing synchronous calls to
JavaScript, whereas in Elm commands (and therefore calls to JavaScript)
are required to be async.

Finally, in Elm, type annotations are optional, but type declarations are
required for custom types. In Mechanical there are no type declarations
even for custom types, thanks to scoped hashtags.

[Like Elm]: https://guide.elm-lang.org/architecture/

#### Flux
[Like Flux], Mechanical is built around unidirectional data flow.
Unlike Flux, Mechanical enforces that views and state updates are pure
functions, and even the imperative-like syntax for side-effects is
referentially transparent. And all of that—views, state, side-effects—are
all typechecked without you having to write a single type annotation.

[Like Flux]: https://facebook.github.io/flux/docs/in-depth-overview

#### Redux
[Like Redux], Mechanical is built around unidirectional data flow with a
single source of truth.
Unlike Redux, Mechanical enforces that reducers and views are pure
functions, and even the imperative-like syntax for side-effects is
referentially transparent. And all of that—reducers, views, side-effects—are
all typechecked without you having to write a single type annotation.

[Like Redux]: https://redux.js.org/understanding/thinking-in-redux/three-principles

#### Functional Core, Imperative Shell
Whereas [Functional Core, Imperative Shell] is just a pattern, Mechanical
enforces a purely functional core model, and the imperative-like syntax
is referentially transparent. Also, updating the UI needn't be part of the
imperative shell, instead the UI is expressed declaratively as a pure
function of state, which is compiled into imperative updates to the UI.

[Functional Core, Imperative Shell]: https://www.destroyallsoftware.com/screencasts/catalog/functional-core-imperative-shell

## License: Blue Oak or MIT

You may use Mechanical under either of our permissive licenses, the highly
readable [Blue Oak Model License](LICENSE-BlueOak.md), or the more standard
[MIT license](LICENSE-MIT).
