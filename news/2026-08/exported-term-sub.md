# An exported `sub term:<foo>` is a term in the importing file

A `sub term:<foo>` makes a bareword `foo` a call to it. A locally declared one
already worked; an *exported* one did not. `apply_module_exports` registers an
export as a user operator only when its name is one of the operator categories,
and `is_operator_sub_name` listed `infix:` / `prefix:` / `postfix:` /
`circumfix:` / `postcircumfix:` but not `term:`. So the export's term symbol was
never registered and the bareword parsed as a plain string:

```raku
use Cro::HTTP::Router;   # exports term:<request> and term:<response>
say request;             # was: the Str "request"
                         # now: X::Cro::HTTP::Router::OnlyInHandler
```

(The inline-`module` import path — `import_inline_module_exports` — already
registered every export's callable term symbol unconditionally, so only the
file-module path was affected.)

Pinned by `t/exported-term-sub.t` with `t/lib/ExportedTerm.rakumod`.

With this, `Cro::HTTP`'s `t/http-router.rakutest` is **fully green (52/52)** — up
from 19-then-abort at the start of the day, via
`news/2026-08/regex-literals-are-closures.md`,
`news/2026-08/pointy-single-literal-parameter.md`,
`news/2026-08/array-alias-survives-a-thread.md` and
`news/2026-08/imported-sub-shadows-io-builtin.md`.
