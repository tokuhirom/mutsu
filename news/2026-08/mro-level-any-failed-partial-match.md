# `.*name`/`.+name` no longer discards successful matches when one MRO level's multi candidates don't cover the call

`Interpreter::resolve_methods_per_mro_level` (`src/runtime/resolution_method.rs`)
is the winner-list builder behind `.*name`/`.+name` (all-candidates dispatch,
used by `call_method_all_with_values`). For a multi method it resolves each
defining MRO level independently; if **any single level** failed to resolve a
matching candidate for the call's arguments, it discarded the entire result
(`return Vec::new()`) — including levels that resolved successfully. The
caller then couldn't tell "no candidate anywhere matched" from "one level's
own candidate set doesn't cover these args," and raised `X::Multi::NoMatch`
even when a real call site should have gotten back the successful matches.

The originally-filed repro (composed roles) turned out not to reproduce on a
clean `main` — role methods flatten into a single class-level candidate set,
so ordinary multi dispatch (which already tries every candidate together)
handles it without ever exercising the per-level path. A plain
class-inheritance shape does exercise it:

```raku
class Base { multi method rt(Numeric $a) { say 'Numeric' } }
class Mid is Base {
    multi method rt()       { say 'empty' }
    multi method rt(Str $a) { say 'Str' }
}
Mid.new.*rt;
```

Real Rakudo prints `empty` (Mid's own `rt()` matches zero args) and THEN
raises a dispatch error for the `Base` level (`Numeric $a` doesn't match zero
args) — the earlier level's side effect happens before the later level's
failure aborts the whole expression. mutsu (before this fix) printed nothing
at all: the pre-flight "does every level resolve" check discarded `Mid`'s
already-successful match before ever invoking anything.

## Fix

`resolve_methods_per_mro_level` now returns `(matches, any_failed)` instead
of collapsing to an empty `Vec` on any failure. The two `call_method_all_with_values`
call sites (instance and type-object paths, `src/runtime/methods_signature_shaped.rs`)
invoke every entry in `matches` first — in MRO order, exactly as before, so
already-resolved levels' side effects run unconditionally — and only *after*
that check `any_failed` to report `X::Multi::NoMatch` instead of a successful
result. This reproduces Rakudo's per-level ordering without needing to
restructure resolution and invocation into a single interleaved loop: since
`matches` is already collected and invoked in MRO order, checking the flag
after the (unchanged) invoke loop is sufficient.

The "totally undefined method" and "every level resolves cleanly" cases are
unaffected (both keep their existing final behavior) — only the new
"partial success, one level's own candidate set doesn't cover the args" case
changes.

Regression tests: `t/mro-all-candidates-multi-partial-match.t`.
