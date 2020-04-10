# Mechanical [![Chat on Gitter](https://badges.gitter.im/mechanical-lang/community.svg)][Gitter]

[Gitter]: https://gitter.im/mechanical-lang/community

A language that makes building webapps (and more!) as easy as building
finite-state machines.

- **Best of both worlds between declarative and imperative:**
  Just say what you want to happen instead of how, by declaratively defining
  your program as a state machine (like [The Elm Architecture]).
  Unlike Elm, straightforwardly perform side-effects using imperative-like yet
  referentially transparent syntax.
- **X% faster and Y% smaller than React:**
  Instead of virtual DOM diffing, just compile to imperative, mutative
  JavaScript.
- **Static types, no type annotations:**
  The benefits of static typing like autocomplete and compile-time error
  checking, never write a single type annotation.
- **Fully interoperable:**
  Compile to JavaScript modules that can call or be called by any JS library
  or browser/Node API.

**Status:** First compile and run of Hello World works&mdash;and absolutely
nothing else. Help wanted!

[The Elm Architecture]: https://guide.elm-lang.org/architecture/

## Example

Mechanical source code:

```
State counter = 0

-- JSX-like view syntax:
View: "#app" <div>
    <p>{counter}</p>
    <button @increment>+1</button>
</div>

-- Event handler:
When receive increment.Click:
  Change counter to counter + 1

```

Compiles to clean, readable JavaScript:

```jsx
render_view("#app", <div>
    <p>{counter}</p>
    <button {...{ "mech-name": "increment" }}>+1</button>
</div>)

const increment = {
    Click: event_stream("increment", "click"),
}

increment.Click.subscribe(() => {
    counter += 1
    p.textContent = counter
})
```

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
- (TODO) `Cmd { ... }`
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

[template literal syntax]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals

#### Incompatibilities

The expression syntax is based on JavaScript's, with a few deliberate
incompatibilities in edge cases that I think JavaScript syntax is confusing:
- Identifiers may only have single, interstitial underscores
    + Valid: `this_is_totally_valid`
    + Invalid: `_foo`, `foo__bar`, `foo_`, `$foo`
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
- No [holes] in arrays (trailing commas are allowed though)

      // valid:
      [ 1, 2 ]
      [ 1, 2, ]
      [
        1,
        2,
      ]

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

- Only arrow function expressions (e.g. `x => 2*x`) are supported (no
  `function (x) { return 2*x }`-style function expressions), functions must
  take at least one argument (all functions are pure, so what would a
  no-argument function do?), and trailing commas aren't allowed
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

## License: Blue Oak or MIT

You may use Mechanical under either of our permissive licenses, the highly
readable [Blue Oak Model License](LICENSE-BlueOak.md), or the more standard
[MIT license](LICENSE-MIT).
