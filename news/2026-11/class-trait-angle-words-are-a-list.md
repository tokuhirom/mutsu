# A custom class/role trait with `<a b>` receives a word list

`model Bla is handling<map grep> { ... }` (Red's `is-handling`) passed the trait the single string
`"map grep"`, so its `for $handling<> { $model.^add_method: $_, ... }` added one method named
`map grep` and `Bla.map(*.a)` fell through to the built-in `map`. A custom `is` trait on a class, role or
package written with angle brackets now gets the words as a list (one word stays a plain string), as a
routine trait already did and as `<map grep>` is anywhere else. Found via Red
(`t/45-is-handling.rakutest`).
