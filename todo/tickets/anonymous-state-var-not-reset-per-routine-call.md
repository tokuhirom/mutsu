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

So `state_scope_id` is not a reliable mid-body lever. Two routes remain.

### Route A — the structural one (give it a real `state_locals` slot)

Give the anonymous form a real local slot and a `state_locals` entry at its
innermost enclosing block, so it uses exactly the named-`state` machinery. That
needs, at minimum: an `is_non_lexical_name` exclusion in `src/opcode.rs`
(otherwise the name becomes a closure-capture candidate and a captured snapshot
races the store), an initializer that yields `Any` rather than `Nil`
(`roast/S03-operators/context.t:87`), and a decision about
`src/vm/vm_call_eligibility.rs`, whose fast/light call paths are gated on
`state_locals.is_empty()` — every sub containing a bare `$` would lose them.

### Route B — classify at compile time, key by the enclosing routine call (2026-08-04)

Not attempted yet, but every row of the table above checks out on paper, and the
levers all already exist. The rule the table encodes is precisely:

> the counter resets iff the `$` is **lexically inside a nested block** that is
> **lexically inside a routine**, and then it resets once per *call of that
> routine* (not per block iteration).

So the classification is static, and only the bucket id is dynamic:

1. **Compile time** — mark each `__ANON_STATE_n__` occurrence as *per-call* when
   it is inside a nested block within a routine. Two existing signals cover the
   two ways a block body reaches the compiler:
   - a body compiled by its own child `Compiler` (`map`/`grep` blocks, pointy
     blocks, `gather`): `!self.is_routine && self.lexically_in_routine` — both
     fields already exist on `Compiler` and are already threaded down by
     `compile_closure_body_with_routine_flag`;
   - a body compiled INLINE into the enclosing chunk (`for`/`while`/`if`/`given`
     bodies): `self.local_scopes.len() > 1`, since frame 0 is the routine/unit
     top level and is never popped, and `push_dynamic_scope_lexical` runs at
     every real block boundary. A `for` STATEMENT MODIFIER has no block and so
     pushes no frame — which is exactly the `3,6` row.

   Record the marked names on `CompiledCode` (e.g. a `per_call_anon_states` set)
   rather than renaming the constant, so no opcode or emit site changes. If a
   pre-pass over the AST is preferred to hooking the emit sites, the set can be
   computed as `all_anon_state_names(body) − shallow_anon_state_names(body)`,
   reusing the block-boundary descent `collect_ph_stmt_shallow` already
   implements (each `__ANON_STATE_n__` is unique per source occurrence, so the
   two sets cannot overlap).

2. **Run time** — `anon_state_key` appends, for a marked name only, the
   invocation id of the innermost enclosing **non-block** routine frame
   (`RoutineFrame::is_block` already distinguishes them; there are only 13
   construction sites, so adding an `invocation_id: u64` stamped from a
   monotonic counter is mechanical). With no such frame — anything at the
   mainline — the id is a constant, so every top-level row keeps persisting.

   Walk through the table: `sub f() { map { ++$ }, 1,2,3 }` marks the `$`
   per-call, the innermost non-block frame is `f`'s, so all three elements
   share a bucket and the next call of `f` gets a new one → `1,2,3` twice.
   `sub p1 { my $x = ++$; $x }` is not inside a nested block → unmarked →
   persists. `my $blk = { ++$ }` is marked but has no routine frame → one
   bucket → persists. `method m { $++ }` is not nested → persists.

   The env-shadowing problem the rejected attempt hit applies here too: a marked
   name must be resolved from the state store only, never from the global env
   entry `GetGlobal`/`SetGlobal` leaves behind.

### Route B, prototyped and shelved (2026-08-04) — the marking must NOT be global

Route B was built end to end and **got every row of the table above right**,
including `3,6`, and made `rmd160("abc")` correct on the second call in a
process. `prove t/` (2865 files) passed. It is shelved for one reason, and the
fix for it is the one thing a next attempt must do differently.

What was built (all of it works, and is worth rebuilding):

- `CompiledCode::anon_state_nested_depth`, a compile-only cursor, plus a hook in
  `CompiledCode::add_constant` — the single choke point every variable-name
  constant passes through, and `__ANON_STATE_<id>__` is used for nothing else,
  so no emit site had to change.
- The cursor set two ways: at child-compiler construction when
  `!is_routine && lexically_in_routine`, and in
  `push_dynamic_scope_lexical`/`pop_dynamic_scope_lexical` for an inline body.
- `Compiler::anon_state_enable_next`, armed by `Stmt::For`'s arm only when
  `!is_statement_modifier`, so ONLY a body proven to be a block counts. This
  opt-in polarity is required: `Stmt::If` and `Stmt::While` carry no
  `is_statement_modifier` flag (adding one means touching ~90 `Stmt::If`
  construction sites), so `$++ if C` — which must keep counting, and which
  `roast/S32-list/rotor.t`'s hand-written `Iterator` depends on — cannot be told
  from `if C { $++ }`. Marking conservatively leaves the unclassifiable shapes
  exactly as they are today.
- `RoutineFrame::invocation_id` + `Interpreter::anon_state_key` folding in the
  innermost non-block frame's id, and `per_call_anon_state_read` prepended to
  the five read chains, answering the site default on a store miss instead of
  falling through to the stale env copy.

**Why it was shelved.** The marked set was kept in a process-global registry,
justified by "the parser mints one id per source occurrence". That is true, but
**the same source is compiled more than once** — the routine-hoist pass,
`record_type_body_captures`' capture analysis (which compiles a class body, and
hence its method bodies, through `compile_closure_body` with `is_routine=false`),
and then the real body — and those passes do not all reproduce the true lexical
context. One pass classifying an id wrongly poisons it permanently, because the
registry is sticky. Concretely: a `$++` in an `if` statement modifier inside a
method inside a class inside a `subtest` block got marked per-call, so its guard
counter reset on every call and `roast/S32-list/rotor.t` hung at test 17.

Making a depth-0 sighting authoritative (an id seen once outside a block is
plain) fixes that hang but breaks the `map { ++$ }` rows the other way — those
ids are *also* seen at depth 0 by one of the passes. The two directions cannot
both be satisfied by a global registry.

**So the marking has to live on the `CompiledCode` that was compiled**, as the
original Route B note said, and the runtime has to consult the *executing*
chunk. That is the one open design question: `anon_state_key` is reached from 13
call sites (`vm_exec_dispatch.rs`, `vm_var_assign_post_incdec.rs`,
`vm_misc_coerce.rs`, `vm_var_assign_typed.rs`, `vm_misc_assign.rs`), none of
which has `code` in hand today. Either thread `&CompiledCode` to them, or keep
an Interpreter-side stack of the executing chunk's per-call set pushed and
popped where chunks are entered.

## Why it matters

Found as the last remaining wrong-digest cause in grondilu's `Digest::RIPEMD`
(`todo/tickets/digest-dist-blockers.md`): its output stage rotates the five
hash words with `map { $_[[^5].rotate(++$)] }`, so the second and later
`rmd160(...)` calls in one process rotate by the wrong amount and return a
correct-but-rotated digest. Each call is correct in a fresh process.
