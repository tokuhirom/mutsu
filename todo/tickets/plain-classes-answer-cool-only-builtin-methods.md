# A plain (non-`Cool`) class answers `Cool`-only builtin methods it should not have

`class G {}; G.new.uc` in real `raku` dies:

```
No such method 'uc' for invocant of type 'G'
```

because a plain class derives from `Any`/`Mu`, not `Cool` — `.uc`, `.flip`,
`.subst`, `.trans`, and the other `Cool`-only builtins (see the set in
`Interpreter::cool_only_builtin_method`,
`src/runtime/methods_native_bypass.rs`) are simply not in `G`'s MRO.

mutsu instead answers `.uc` (and the rest of that set) for **any** Instance
receiver, regardless of whether its MRO actually includes `Cool` — the
native fast path (`try_native_method_raw` in `src/vm/vm_native_dispatch.rs`,
and the by-name dispatcher in `methods_call_dispatch.rs`) stringifies the
receiver and applies the builtin unconditionally. So `G.new.uc` in mutsu
succeeds, returning `"G()"` (the default stringification, upper-cased),
instead of dying.

## Where this was found

Found while implementing
`todo/tickets/wildcard-handles-loses-to-builtin-cool-methods.md`'s fix
(now closed — see `news/2026-08/`). That ticket's own test file
(`t/handles-wildcard-builtin-methods.t`) has two `todo`-marked assertions
that depend on this: a class delegating (via `handles *`) to a *plain*
class (`Bare.new`) for a `Cool`-only method like `.uc` should see the
delegate die, so the delegating class's own `FALLBACK`/error path gets a
chance to run — but since `Bare.new.uc` wrongly succeeds in mutsu, that
never happens (the wildcard block returns the delegate's wrong answer
directly instead of falling through).

## Why this is a separate, larger ticket

This is not scoped to `handles *`/`FALLBACK` interception at all — it is
about whether `Cool`-only methods should be gated behind "does this
receiver's class actually have `Cool` in its MRO" universally, for *every*
Instance, not just ones with a wildcard delegate. Fixing it properly likely
means adding a `class_mro_includes_cool` check (or similar) to the same two
gates the wildcard-handles fix added
(`try_native_method_raw`'s Instance arm, `methods_call_dispatch.rs`'s
`shadows_builtin` check) — but unconditionally, not just when
`class_has_wildcard_handles_or_fallback` is true. That is a much larger
blast radius: every plain-class instance in the whole test suite that
happens to call a `Cool`-only method name would newly need to either (a)
genuinely have `Cool` composed somewhere in its MRO (most user classes
composing `Cool` via an explicit `is Cool` or similar — verify this is
actually common/expected), or (b) start dying where it previously silently
"worked" via the wrong stringify fallback — a correctness fix that could
easily also be a regression risk across many roast files that never
exercised this distinction. Needs its own investigation pass (how common is
an accidental `.uc`-shaped call on a plain instance across `t/`/`roast/`
today, and whether `raku`'s builtin/prelude classes actually do compose
`Cool` in a way mutsu's registry doesn't yet track) before attempting a
fix — not a quick follow-up to the wildcard-handles PR.

## Repro

```raku
class G {}
say G.new.uc;   # raku: dies "No such method 'uc' for invocant of type 'G'"
                # mutsu: "G()"
```
