# A typed store stops re-deriving its constraint's resolution

[#8819](https://github.com/tokuhirom/mutsu/issues/8819) measured mutsu at ~860
machine instructions per executed bytecode opcode and named
[#8820](https://github.com/tokuhirom/mutsu/issues/8820) — `SetLocal`'s
name-derived work — as its largest single site. Taking that slice started with
re-running its own measurement, and the profile pointed somewhere slightly
different from where the issue expected.

## What the profile actually said

`callgrind` on the issue's own benchmark, release-optimized with debuginfo
(`cargo build --profile profiling`):

```raku
sub go() { my int $i = 0; my int $n = 300000; while $i < $n { $i = $i + 1 }; $i }
```

`exec_set_local_op_inner` is 51% of the whole program inclusive — 2,528
instructions per store. But almost none of that is the store cascade itself,
and almost none of it is the sigil string tests the issue's slice A proposed
to replace with slot metadata. Those tests cost **11 instructions per store**,
0.2% of the program: rustc had already hoisted and CSE'd the ~50
`name.starts_with('@')` calls scattered through the 2,000-line function into a
handful of byte loads. Rewriting all fifty of them onto a compile-time
descriptor would have been a large diff for nothing measurable.

The cost is in what the store does *after* it has found the constraint:

| callee of `exec_set_local_op_inner` | Ir per store | share of program |
| --- | --- | --- |
| `type_matches_value` | 639 | 12.9% |
| `try_coerce_value_for_constraint` | 411 | 8.3% |
| `wrap_native_int_by_constraint` | 104 | 2.1% |

23% of a program that does nothing but compare, add and store an integer,
spent deciding facts about a constraint spelled `int` — the same three bytes,
300,000 times.

## The three sites

**`resolved_type_capture_name` cost ~416 instructions for a three-byte name.**
It ran three separate pattern searches over the constraint — `find([':', '('])`
(a generic `CharSearcher`, not a `memchr`), `find('[')`, and
`contains("::")` — probed `env` twice for a `::T` capture binding, and finished
by allocating a `String` copy of the name it had been handed. It now scans the
bytes once for all three structural questions, skips both capture arms outright
when no `::T` capture has ever been bound in the process (the same
`TYPE_CAPTURE_SEEN` latch `has_type_capture_binding` already consults, hoisted
so it is read once rather than once per arm), and returns `Cow::Borrowed` when
the name resolves to itself. Borrowed *means* "resolved to itself", so
`type_matches_value` matches on the variant instead of comparing the bytes
back. The old `String`-returning entry point stays, as a one-line wrapper over
the `Cow` one, for the ~25 call sites that want an owned name — there is no
second implementation to drift.

**`try_coerce_value_for_constraint` resolved the constraint before asking
whether it had anything to do with it.** `resolve_constraint_alias` is a
`Symbol::lookup` (a thread-local `HashMap<String, _>` probe, ~159 instructions)
plus an `Env::get_sym` (~115), and it ran on every typed assignment — ahead of
the `registry().subsets.is_empty()` test that returns the value untouched.
Past the coercion arm a subset redirect is the only thing that can still change
the value, and an alias hop cannot produce one: `resolve_constraint_alias`
answers with a *package* name, which carries no `(...)` and so cannot re-enter
the coercion arm on the recursion. Every alias hop lands back at the same
early return. Asking `is_empty` first is the same answer for one load.

**`type_matches_value`'s three fast accepts probed `registry().subsets` by
key**, and a `HashMap` hashes its key before it can discover the map is empty.
`is_subset_type_name` puts the `is_empty` test in front of the hash.

## Result

Same benchmark, same binary shape, `MUTSU_VM_STATS` opcode counts identical in
both JIT configurations (150,317 with the JIT on, 240,023 with `MUTSU_JIT=off`)
— this changes how much each opcode costs, not how many run:

| | before | after |
| --- | --- | --- |
| `type_matches_value` | 639 Ir/store | 194 Ir/store |
| `try_coerce_value_for_constraint` | 411 Ir/store | 64 Ir/store |
| `exec_set_local_op_inner` (inclusive) | 2,528 Ir/store | 1,743 Ir/store |
| whole program | 148,784,763 Ir | 125,991,043 Ir |

**−15.3% of the program's retired instructions**, from three local changes that
alter no semantics. None of it is specific to `int`: any declared type on any
assignment, in any program with no `subset` and no bound `::T`, takes the same
path.

`t/types/type-constraint-name-resolution.t` pins the arms that have to stay
reachable — a `subset` predicate, a coercion type, a `::T` capture, a
`constant` type alias inside a parameterization, a package-scoped nested type —
each of the six behaviours checked against rakudo first.

## What this leaves

The slice-A finding is worth recording so nobody spends a PR on it: the sigil
tests are already free, and `CompiledCode`'s slot descriptor (ADR-0097) does
not need a sigil field for this reason. The remaining per-store tax is spread
thin again — `is_identity_scalar_restore`, `update_bound_decont_marker`,
`mirror_attr_local_to_cell`, `normalize_scalar_assignment_value`,
`resolve_pending_alias_binds` and `reset_atomic_var_key` are ~28 instructions
each and all run on a store that needs none of them, which is the shape
[#8819](https://github.com/tokuhirom/mutsu/issues/8819) describes and the
existing `exec_set_local_scalar_fast` gate exists to bypass. That gate declines
a typed lexical outright (`env_type_constraint_seen_for`), which is why this
benchmark never reaches it — extending it to serve a typed native scalar is the
next slice, and is now worth substantially less than it was before this change.
