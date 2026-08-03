# An anonymous state variable (`$++` / `++$`) is not reset when its enclosing routine is re-entered

A bare `$` in a block is an anonymous state variable belonging to that block's
*clone*. Re-entering the enclosing routine produces a fresh clone, so the counter
restarts. mutsu keys it only by its compile-time name, so it keeps counting
across calls:

```raku
sub f() { (map { ++$ }, 1, 2, 3).join(',') }
say f();   # raku: 1,2,3   mutsu: 1,2,3
say f();   # raku: 1,2,3   mutsu: 4,5,6

sub g() { my @r; for ^3 { @r.push(++$) }; @r.join(',') }
say g();   # raku: 1,2,3   mutsu: 1,2,3
say g();   # raku: 1,2,3   mutsu: 4,5,6

sub h() { (map { $++ }, 1, 2, 3).join(',') }
say h();   # raku: 0,1,2   mutsu: 0,1,2
say h();   # raku: 0,1,2   mutsu: 3,4,5
```

A **named** `state` in the same position is already correct — `sub i() { (map {
state $n = 0; ++$n }, 1, 2, 3).join(',') }` yields `1,2,3` twice in both — so the
fix is to give the anonymous form the same per-clone keying the named one has.
A block stored in a variable and called repeatedly (`my $blk = { ++$ }; $blk()`)
must keep counting: that is one clone.

## Where

`Interpreter::anon_state_key` (`src/vm/vm_var_ops.rs`) builds the key as
`__anon_state::__ANON_STATE_<n>` — the compile-time placeholder name and nothing
else — and `anon_state_value` / `sync_anon_state_value` read and write it in the
process-wide `state_vars` map. Named `state` instead goes through
`CompiledCode::state_locals` plus `normalize_state_key`
(`src/runtime/runtime_class_query.rs`), which is what gives it a per-invocation
identity. The readers are `vm_exec_dispatch.rs` (the `__ANON_STATE__` fast path),
`vm_misc_coerce.rs`, `vm_var_assign_post_incdec.rs` and `vm_var_assign_typed.rs`.

## Why it matters

Found as the last remaining wrong-digest cause in grondilu's `Digest::RIPEMD`
(`todo/tickets/digest-dist-blockers.md`): its output stage rotates the five
hash words with `map { $_[[^5].rotate(++$)] }`, so the second and later
`rmd160(...)` calls in one process rotate by the wrong amount and return a
correct-but-rotated digest. Each call is correct in a fresh process.
