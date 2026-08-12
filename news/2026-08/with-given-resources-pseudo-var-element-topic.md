# `with`/`given` binding `%?RESOURCES{key} -> $v` to Nil instead of the real resource

`%?RESOURCES` is a compiler-synthesized pseudo-hash (rebuilt fresh on every
plain read from the current package's distribution via
`build_resources_for_package`), not a real container stored in locals/env.
`with`/`given`'s "element-source" writeback optimization for a subscripted
lvalue topic (`with %h<k> -> $v {...}`, used so `$_ = ...`/`.push` mutations
propagate back to the source element) resolves the base container by name in
the locals store — which finds nothing for `%?RESOURCES` and silently binds
the topic to `Nil` instead of falling through to a plain (read-only, but
correct) element read.

```raku
with %?RESOURCES{'greeting.txt'} -> $resource {
    say $resource.IO.slurp;   # raku: file contents — mutsu (before fix): dies on Nil.IO
}
```

`Cro::HTTP::Router`'s bundled-resource routes use exactly this shape
internally. Fixed by excluding `%?RESOURCES` from the element-source
writeback optimization in `container_var_name`
(`src/compiler/helpers_control_flow.rs`) — mirroring the existing exclusion
for instance-attribute containers (`%!h`/`@!a`) right above it, which hit the
same class of bug for the same reason (not a real locals-store variable).
Pin: `t/with-resources-pseudo-var-element.t` (new fixture distribution
`t/lib/ResElemTopic/`).

Found while diagnosing
`todo/tickets/static-resource-content-type-mismatch-and-related-failures.md`
(a `t/http-router.rakutest` failure cluster) — this fix did not resolve that
ticket's remaining 404s, which come from a separate, still-open mechanism
(see the ticket for the narrowed investigation).

A related but distinct gap was found alongside this fix and filed
separately, not fixed here:
`todo/tickets/given-with-explicit-rw-pointy-param-element-topic-no-writeback.md`
(an *explicit* `-> $v is rw` pointy parameter on an element-source topic does
not write back, unlike the implicit `$_` mutation form).
