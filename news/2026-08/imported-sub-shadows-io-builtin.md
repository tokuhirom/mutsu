# An imported sub shadows the IO builtin of the same name

`say` / `print` / `put` / `note` have a builtin *statement* form in the parser,
tried before the general listop-call parse. A locally declared `sub say(...)`
already bailed out of it (`shadowed_by_user_sub`), so the user sub won — but an
**imported** one did not, because `use`d exports land in the scope's
`imported_functions` set, not in `user_subs`, and only the latter was consulted.

```raku
use Cro::HTTP::Router;   # exports the HTTP verb `put`
put -> 'product' { ... } # parsed as `put(the-block)` — a PRINT, not a route
```

The lexical scope does not care where a binding came from, so
`shadowed_by_user_sub` now consults `is_imported_function` as well.

Pinned by `t/imported-sub-shadows-io-builtin.t` (with `t/lib/IOBuiltinShadow.rakumod`).

Concretely, this is why `Cro::HTTP`'s five-verb route set registered only four
routes: `put -> 'product' { … }` printed the block instead of adding the PUT
handler, so a PUT request 405'd.
