# Bareword `hash` now calls the zero-arg `hash` builtin

A bareword `hash` (no parens, no arguments) previously evaluated to the
string `"hash"` instead of calling the `hash` listop. Raku accepts `hash`
called bare (unlike `set`/`bag`, which it rejects), returning an empty
hash — e.g. `%h = hash;` or `say hash.raku`.

## Root cause

`Interpreter::is_implicit_zero_arg_builtin` (`src/runtime/registration.rs`)
is the allowlist that decides whether a bareword is treated as an
implicit zero-arg builtin call versus falling through to the
bareword-as-string fallback. It only listed `dir` and `lines`; `hash` was
missing, so `OpCode::GetBareWord` fell through and stringified it. This
surfaced as a real bug in `t/http-session-persistent.rakutest`: the test's
`purge` method body is `%!fake-db = hash;`, which died with "Odd number of
elements found where hash initializer expected", aborting the file mid-run
with no TAP plan.

## Fix

Added `"hash"` to `is_implicit_zero_arg_builtin`. `hash()` with parens
already worked, and this predicate is shared by the eval-context name
check (`system_eval_names.rs`) and the bareword dispatch
(`vm_var_get_ops.rs`), so no other site needed a change. A user-declared
`sub hash { ... }` still shadows the builtin, matching the existing
`dir`/`lines` behavior. `set`/`bag` were deliberately NOT added — raku
itself rejects those called bare.

## Verification

- `mutsu -e 'my %h = a => 1; %h = hash; say %h.elems'` now prints `0`,
  matching raku exactly.
- `mutsu -e 'say hash.raku'` now prints `{}`, matching raku.
- New pin: `t/hash-term.t`.
- Full `make test` (2986 files) and the whitelisted subset/uint/enum
  roast files pass with no regressions.
