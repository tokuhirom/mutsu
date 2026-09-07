# A package-qualified call no longer retries under its short name

```raku
sub zzz($n) { $n + 1 }
say NoSuchPkg::zzz(1);
# raku:  Could not find symbol '&zzz' in 'GLOBAL::NoSuchPkg'
# mutsu: 2
```

`Pkg::name(...)` resolved to the caller's own `name` whenever `Pkg` declared no
such routine — even when `Pkg` did not exist at all. That is a **silent
wrong-routine dispatch**, not merely a missing error: a typo'd or stale package
qualifier ran a same-named routine and returned a plausible answer.

## Root cause

`call_function_fallback` ended a qualified call by stripping the package prefix
and retrying under the short name. An earlier round
(`t/qualified-call-does-not-alias-builtin.t`) had already narrowed the strip so
it could not reach *builtins* — `Foo::Bar::index("hello", "l")` used to return
`2` — by gating it on "mutsu has something declared under this short name". But
a user routine IS something declared under that short name, so the gate let the
wrong-routine case straight through.

The strip is now gone entirely. Nothing legitimate depended on it: a package
mutsu *does* know has already had its chance through the registry, where a
module's `our sub` is found and its plain `sub` is correctly `my`-scoped and
invisible — which is exactly how raku scopes them. The narrower retry for a
short-name-registered *type* (`Foo::Bar("x")`, `Foo::E(1)`) went with it, having
been measured as divergent too: raku refuses those.

## Why it took a full pass rather than a spot check

The ticket predicted fallout, since the qualified path is also how a `unit
module`'s own `our sub` is reached in several shapes. There was none: `make
test` (3775 files, 39610 tests) and the 232 whitelisted
`roast/S06-*`, `S10-packages`, `S11-modules`, `S12-*` and `S02-names-vars` files
(5790 subtests) are green with the strip removed.

## Scope

`t/qualified-call-does-not-alias-builtin.t` grows three rows (14 total): the
same-named user routine, a short-name-registered enum, and a short-name-
registered class. Its existing regression guards — a qualified `our sub`, a
two-level package path, a qualified multi, and a qualified name reached through
`EVAL` — all still resolve, through the registry.

This also unblocks the assertion
`todo/deep/module-toplevel-private-sub-leak-cleanup.md` wanted and could not
write: "a compunit-private routine is not reachable as `Mod::name`" was
untestable while the caller's own same-named routine answered the qualified
call.
