# Bareword `hash` is stringified instead of calling the `hash` listop

## Affected tests
- `t/http-session-persistent.rakutest`: after subtest 16 the test calls `$persistent.purge`, whose body is `%!fake-db = hash;`. mutsu dies with `Odd number of elements found where hash initializer expected: found 1 element(s); last element seen: hash`, aborting the file — this is why the run has NO TAP plan and rc=1 (raku's `prove` reports it as a bad plan on top of the 4 subtest failures).

## Repro (verified)
```raku
my %h = a => 1, b => 2;
%h = hash;      # raku: empty hash
say %h.elems;
```
- raku: `0` (and `say hash.raku` → `{}`)
- mutsu: `Odd number of elements found where hash initializer expected: ... last element seen: hash` — the bareword evaluated to the string `"hash"`.

`hash()` with parens works in mutsu (`{}`, and `hash(a => 1)` → `{:a(1)}`). Bare `list` works in mutsu. Note raku itself REJECTS bare `set`/`bag` ("may not be called without arguments") but ACCEPTS bare `hash` and `list` — mirror that: only `hash` needs adding, do not add `set`/`bag`.

Secondary observation (cosmetic, optional): mutsu attributes the error to "in sub purge at t/http-session-persistent.rakutest line 48", but `purge` is at line 63-65; line 48 is inside `load`. Wrong line attribution for method bodies in monitors.

## Root cause
The parser produces `Expr::BareWord("hash")` (`mutsu --dump-ast -e 'my %h = hash;'` shows `VarDecl { expr: BareWord("hash") ... }`). At runtime `OpCode::GetBareWord` (`src/vm/vm_exec_dispatch.rs:741` → `exec_get_bare_word_op` in `src/vm/vm_var_get_ops.rs`) only treats a bareword as an implicit zero-arg builtin call when `Interpreter::is_implicit_zero_arg_builtin(name)` says so (`src/vm/vm_var_get_ops.rs:267`), and that allowlist is just `dir` and `lines`:

```rust
// src/runtime/registration.rs:842
pub(crate) fn is_implicit_zero_arg_builtin(name: &str) -> bool {
    matches!(name, "dir" | "lines")
}
```

`hash` is not in the list, so the bareword falls through to the string fallback. (`list` works via a different path, so only `hash` is missing.)

## Fix direction
Add `"hash"` to `is_implicit_zero_arg_builtin` (`src/runtime/registration.rs:842`). The same predicate is also consulted by `src/runtime/system_eval_names.rs:863`. Confirm the call it routes to lands on the existing `hash` builtin (the one `hash()` already reaches — `builtin` dispatch in `src/runtime/builtins*.rs`). Make sure user-declared subs named `hash` still shadow the builtin (the existing zero-arg-builtin path already has that ordering for `dir`/`lines`; keep it).

Do NOT add `set`/`bag`/`Set` etc. — raku rejects those bare (verified), and `hash` is listed in `raku-doc/doc/Language/perl-func.rakudoc`'s builtin set, satisfying the "builtins must be in perl-func" rule.

Risks: minimal; a bareword `hash` used as a string was never valid Raku. Grep `t/` for accidental reliance.

## Verification
- `target/debug/mutsu -e 'my %h = a => 1; %h = hash; say %h.elems'` prints `0`; `-e 'say hash.raku'` prints `{}`.
- `t/http-session-persistent.rakutest` no longer aborts after subtest 16: TAP plan `1..16` is printed and prove reports a clean plan (subtests 8/9/13/16 still need the sibling tickets).
- Add a `t/` pin, e.g. `t/hash-term.t` (bare `hash` in assignment, `say hash.raku`, and `hash(a=>1)`).
