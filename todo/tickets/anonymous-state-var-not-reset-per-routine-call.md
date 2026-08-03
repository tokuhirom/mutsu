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

## The scope rule, and what it has to preserve

The counter belongs to the **innermost enclosing block's clone**. A named sub's
body is cloned once (at registration), so a bare `$` directly in a sub body
*persists* across calls; a `map`/`for` block inside it is cloned per call, so a
`$` there *resets*. Both directions are load-bearing:

| shape | raku | must |
|---|---|---|
| `sub f { my $x = ++$; $x }` x3 | `1,2,3` | persist (`roast/S02-types/whatever.t:486`) |
| `sub k() { $c = ++$ for ^3; $c }` x2 | `3,6` | persist (statement modifier, no block) |
| `my $blk = { ++$ }` x3 | `1,2,3` | persist (one clone) |
| `method m { $++ }` x3 | `0,1,2` | persist (`roast/S32-list/rotor.t:68`) |
| `[ $++ xx 3 ] xx 3` | `0..8` | persist (`roast/S04-statements/gather.t:242`) |
| `while $++ < 3` | - | persist across iterations |
| `sub f() { map { ++$ }, 1,2,3 }` x2 | `1,2,3` twice | **reset** |
| `sub g() { for ^3 { ... ++$ } }` x2 | `1,2,3` twice | **reset** |

## A tried-and-rejected fix (2026-08-04)

Routing `anon_state_key` through `Interpreter::scoped_state_key` — the clone-id
scoping a named `state` uses — plus resolving these names *only* from the state
store (their `env` entry is global via `GetGlobal`/`SetGlobal`, so it outlives
the clone and was being found first) fixes every reset row above and keeps the
one-clone, method, `xx`-thunk, `while` and grid rows. **But it breaks the two
named-sub persistence rows:** `sub f { my $x = ++$; $x }` yields `1,1,2` and the
statement-modifier `for` yields `3,3`. The `1,1,2` shape says `state_scope_id`
alternates between two values across successive calls of one named sub when read
*mid-body* — a named `state` in the same position is unaffected because it only
consults the id at the call boundaries (`load_state_locals` /
`sync_state_locals`), never inside the body.

So `state_scope_id` is not a reliable mid-body lever. The remaining route is the
structural one: give the anonymous form a real local slot and a `state_locals`
entry at its innermost enclosing block, so it uses exactly the named-`state`
machinery. That needs, at minimum: an `is_non_lexical_name` exclusion in
`src/opcode.rs` (otherwise the name becomes a closure-capture candidate and a
captured snapshot races the store), an initializer that yields `Any` rather than
`Nil` (`roast/S03-operators/context.t:87`), and a decision about
`src/vm/vm_call_eligibility.rs`, whose fast/light call paths are gated on
`state_locals.is_empty()` — every sub containing a bare `$` would lose them.

## Why it matters

Found as the last remaining wrong-digest cause in grondilu's `Digest::RIPEMD`
(`todo/tickets/digest-dist-blockers.md`): its output stage rotates the five
hash words with `map { $_[[^5].rotate(++$)] }`, so the second and later
`rmd160(...)` calls in one process rotate by the wrong amount and return a
correct-but-rotated digest. Each call is correct in a fresh process.
