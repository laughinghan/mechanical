# Mechanical

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

**Note:** very incomplete and buggy, don't rely on it actually working.
**Help wanted!**

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

## License: Blue Oak or MIT

You may use Mechanical under either of our permissive licenses, the highly
readable [Blue Oak Model License](LICENSE-BlueOak.md), or the more standard
[MIT license](LICENSE-MIT).
