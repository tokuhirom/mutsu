# `Version` keeps the source spelling of its parts

`v1.02.3` stringified as `1.02.3` in Rakudo but as `1.2.3` in mutsu: a `Version`
was stored purely as its parsed parts (`(1, 2, 3)`), and every stringification
(`.Str`, `~`, `.gist`, `.raku`, hash-key coercion) rebuilt the text from those
parts, throwing away the zero padding.

Rakudo keeps the original text of each part while `.parts` still reports the
parsed `Int`s, so `Version.new("1.02.3").Str` is `"1.02.3"` and
`Version.new("1.02.3").parts` is `(1, 2, 3)`. Only the *separators* are
normalized (`Version.new("1.2-beta").Str` is `"1.2.beta"` in both).

`ValueRepr::Version` now carries an optional `text` field with the source
spelling of the parts joined by `.`. It is populated only when it differs from
what the parts alone render to, so the overwhelmingly common `v1.2.3` case stays
allocation-free and byte-identical to before. All three string-derived
constructors (the `v…` literal in the parser, `Version.new(Str)`, and the
`.Version` coercion) route through a new `Value::version_from_str`.

Two follow-on parity fixes fell out of it, both of which were wrong before and
only became *observable* once versions could differ by spelling:

- **`.WHICH`** had no `Version` arm and fell through to the global-counter
  fallback, yielding a meaningless `Version|1`. It is now
  `Version|<canonical string>` (a `ValueObjAt`, as in Rakudo).
- **`===`** fell back to `eqv`, so `v1.02.3 === v1.2.3` was `True`. Identity is
  the `.WHICH` string, so it is `False` now — while `==`, `cmp` and `eqv` all
  still see the two as equal, exactly as Rakudo does.

Found while triaging `TODO_dist` ticket T-046 (RakudoPrereq), whose test asserts
the error message contains the compiler version `v2017.03.290`.

Pin: `t/version-source-spelling.t` (22 assertions, passes under both mutsu and raku).
