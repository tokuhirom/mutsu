# A coercion to the type a mixed value already is keeps the mixin

rakudo's core types answer a coercion to a type they already are with
`self`: `method Int(Int:D:) { self }`, `Str.Str`, `Num.Numeric`, and so on.
So `(5 but R).Int` still does `R`, and so does `+(5 but R)`. Likewise
`my $x = 5 but "five"; say +$x` prints `five`. mutsu unwrapped the mixin and
answered the plain value.

The identity coercions are now one table (`value::mixin_identity`):

- Int: `Int`, `Numeric`, `Real`
- Num: `Num`, `Numeric`, `Real`
- Rat: `Rat`, `Numeric`, `Real`
- Complex: `Complex`, `Numeric`
- Str: `Str`
- Bool: `Bool`

A `but`-mixed value answers those with itself, by method or by prefix `+`,
`~` or `?`. Three cases still produce a new value:

- a coercion that builds one, such as `Int.Num`;
- an allomorph (`+<42>` is the plain `Int`);
- a role that declares the coercion method itself.

Two local tests assumed the old behaviour and were brought in line with
rakudo. A role `Str` that interpolated `self.Numeric` now recurses, as it
does in rakudo, and `+(1 but True)` is no longer `is-deeply 1`.
