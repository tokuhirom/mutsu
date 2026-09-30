# Exported `my class` / `my constant` in a parameterized role body are importable at load

`unit role V6[::K]; my class Foo is export { }; my constant X is export = 5;`
now exports `Foo` and `X` the moment the module is loaded, as Rakudo does.
Previously only a non-parameterized role's body declarations were registered
eagerly (#9981); a parameterized role deferred every `my class`/exported
`my constant` to composition, so `use V6; say Foo.^name` printed `Str`.

The eager pass now also runs in a parameterized role for any declaration that
does not mention one of the role's parameters (checked by walking the
statement's AST). A parameter-mentioning declaration still waits for
composition to bind the parameter. As a side effect, a parameter-free nested
class is a single `R::C` shared by `R[Int]` and `R[Str]`, matching Rakudo,
rather than a per-composition `R::C[Int]` copy (#10244).
