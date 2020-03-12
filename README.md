# Mechanical: a state machine-oriented programming language

- Programming in Mechanical is just defining parameterized state machines
  (aka [The Elm Architecture]), so understanding the state of the program and
  how it can change is **as easy as finite-state machines**.
- Unlike Elm, event handlers can update state and perform side-effects in an
  **imperative-like syntax**.
- There are **no type or effect annotations,** but Mechanical is **statically typed**
  and the imperative-like syntax is **referentially transparent**, thanks to a
  fully inferred algebraic type and effect system.

Mechanical compiles to an extremely readable, understandable, wicked-fast JavaScript
modules that are fully interoperable with the rest of the JavaScript ecosystem.

**Note:** alpha-quality software, incomplete, probably buggy. Don't rely on it
to actually work. **Help wanted!**

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

## License: BlueOak-1.0.0 or MIT

You may use Mechanical under either of our permissive licenses, the highly
readable [Blue Oak Model License](LICENSE-BlueOak.md), or the more standard
[MIT license](LICENSE-MIT).
