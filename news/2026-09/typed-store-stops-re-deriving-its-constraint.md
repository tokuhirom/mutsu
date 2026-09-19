# A typed store stops re-deriving its constraint's resolution

[#8819](https://github.com/tokuhirom/mutsu/issues/8819) measured mutsu at ~860
machine instructions per executed bytecode opcode and named
[#8820](https://github.com/tokuhirom/mutsu/issues/8820) — `SetLocal`'s
name-derived work — as its largest single site. Taking that slice started with
re-running its own measurement, and the profile pointed somewhere slightly
different from where the issue expected. It also found the more useful half of
the ticket, which is a *negative* result.

## Slice A is not worth a PR, and now nobody has to find that out twice

The issue's slice A proposed replacing `exec_set_local_op_inner`'s ~50
`name.starts_with('@')` / `('%')` / `('&')` sigil tests with a compile-time
classification on `CompiledCode`'s slot descriptor (ADR-0097's `BindingDesc`,
whose `is_simple_scalar_local` is the existing precedent).

`callgrind` on the issue's own benchmark says those tests cost **11
instructions per store, 0.2% of the program**. Only five `starts_with` lines in
the whole 2,000-line function carry any cost at all; rustc has already hoisted
and CSE'd the rest into a handful of byte loads, and a `starts_with(char)`
never reaches a `memcmp` in the first place — it is a length check and a byte
compare. A ~50-site rewrite with a silent failure mode, for 0.2%, is the shape
`CLAUDE.md` calls "the appearance of progress through small safe diffs". **So
`BindingDesc` does not need a sigil field**, and ADR-0097 does not have to grow
one.

## Where the cost actually is

```raku
sub go() { my int $i = 0; my int $n = 300000; while $i < $n { $i = $i + 1 }; $i }
```

`exec_set_local_op_inner` is 51% of the whole program inclusive — 2,528
instructions per store — and almost all of it is what the store does *after* it
has found the constraint:

| callee of `exec_set_local_op_inner` | Ir per store | share of program |
| --- | --- | --- |
| `type_matches_value` | 639 | 12.9% |
| `try_coerce_value_for_constraint` | 411 | 8.3% |
| `wrap_native_int_by_constraint` | 104 | 2.1% |

23% of a program that does nothing but compare, add and store an integer,
spent deciding facts about a constraint spelled `int` — the same three bytes,
300,000 times. The env probe slice B was aimed at (`Env::get_sym`) is real but
is only 3.6%; what dominates is re-parsing the constraint *string* after the
probe has returned it.

## Converging with #8815 / #8827

Halfway through this slice,
[#8827](https://github.com/tokuhirom/mutsu/pull/8827) landed on `main` having
found the same allocation in the same function from the `bench-array` side. It
fixed it by splitting `resolved_type_capture_name` into a core returning
`Option<String>` (`None` means "resolves to itself", so the identity answer
allocates nothing) plus a caller-side disjunction in `type_matches_value` that
skips the call entirely for a plain constraint. That is the same idea as this
branch's `Cow<'_, str>` version, so the `Cow` layer was **dropped** in the
rebase rather than stacked on top — two abstractions for one fact is exactly
the drift `CLAUDE.md` warns about. What survives is what is still additive.

## The three changes

**`try_coerce_value_for_constraint` resolved the constraint before asking
whether it had anything to do with it.** `resolve_constraint_alias` is a
`Symbol::lookup` (a thread-local `HashMap<String, _>` probe, ~159 instructions)
plus an `Env::get_sym` (~115), and it ran on every typed assignment — ahead of
the `registry().subsets.is_empty()` test that returns the value untouched. Past
the coercion arm a subset redirect is the only thing that can still change the
value, and an alias hop cannot produce one: `resolve_constraint_alias` answers
with a *package* name, which carries no `(...)` and so cannot re-enter the
coercion arm on the recursion. Every alias hop lands right back at the same
early return. Asking `is_empty` first is the same answer for one load. This is
untouched by #8827 and is the bulk of the win.

**`type_matches_value`'s three fast accepts probed `registry().subsets` by
key**, and a `HashMap` hashes its key before it can discover the map is empty.
`is_subset_type_name` puts the `is_empty` test in front of the hash.

**`try_resolved_type_capture_name` ran three separate pattern searches over the
same few bytes** — `find([':', '('])` (a generic `CharSearcher`, not a
`memchr`), `find('[')` and `contains("::")` — and probed `env` twice for a
`::T` capture binding. One byte scan now answers all three structural
questions, and both capture arms are skipped outright when no `::T` capture has
ever been bound in the process (`any_type_capture_seen`, the latch #8827 added,
asked once here instead of once inside each `has_type_capture_binding`).
#8827's caller-side guard means `type_matches_value` no longer reaches this
function for a plain constraint, so this half is not what moves the benchmark
any more — it is for every other caller (parameter binding, method dispatch,
the typed-store cascade), and for any program that *does* declare a
package-scoped nested type, where that guard is always true.

## Result

Measured against `main` at `6fe7c864` (i.e. with #8827 already in), same
benchmark, same binary shape. `MUTSU_VM_STATS` opcode counts are identical
before and after in **both** JIT configurations (150,317 with the JIT on,
240,023 with `MUTSU_JIT=off`) — this changes how much each opcode costs, not
how many run, which is the acceptance #8820 asks for.

| | before (`6fe7c864`) | after |
| --- | --- | --- |
| `try_coerce_value_for_constraint` | 420 Ir/store | **64 Ir/store** |
| `type_matches_value` | 114 Ir/store | 107 Ir/store |
| `wrap_native_int_by_constraint` | 103 Ir/store | 103 Ir/store |
| `exec_set_local_op_inner` (inclusive) | 2,019 Ir/store | 1,656 Ir/store |
| whole program | 134,086,977 Ir | **123,194,058 Ir** |

**−8.1% of the program's retired instructions.** The split is worth reading
honestly: −10.68M of the −10.89M is the `try_coerce_value_for_constraint`
reordering alone, −0.21M is the subset-probe guard, and the byte scan
contributes **nothing measurable here** — #8827's caller-side guard already
keeps this benchmark out of that function, exactly as described above. It is
kept because every other caller still enters it, not because it moves this
number.

None of it is specific to `int`: any declared type on any assignment, in any
program with no `subset` and no bound `::T`, takes the same path.

`t/types/coercion/type-constraint-name-resolution.t` pins the arms that have to
stay reachable — a `subset` predicate, a coercion type, a `::T` capture, a
`constant` type alias inside a parameterization, a package-scoped nested type —
each of the six behaviours checked against rakudo first.

## What this leaves

The remaining per-store tax is spread thin again:
`is_identity_scalar_restore`, `update_bound_decont_marker`,
`mirror_attr_local_to_cell`, `normalize_scalar_assignment_value`,
`resolve_pending_alias_binds` and `reset_atomic_var_key` are ~28 instructions
each and all run on a store that needs none of them, which is the shape
[#8819](https://github.com/tokuhirom/mutsu/issues/8819) describes and the
existing `exec_set_local_scalar_fast` gate exists to bypass. That gate declines
a typed lexical outright (`env_type_constraint_seen_for`), which is why this
benchmark never reaches it — extending it to serve a typed native scalar is the
next slice, and is now worth substantially less than it was before this change.
