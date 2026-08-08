# A multi-parameter `for` loop with an `@`/`%`-sigil parameter mutates the enclosing container it shadows

**Update 2026-08-08:** the scalar case is *not* fully fixed either — the earlier
fix only covered a shadowed name that already had a local slot in the same
frame. When the frame has no slot for the name, the bind falls through to a
**global by-name write** and clobbers an outer lexical of that name:

```raku
sub f() { for <a b c>.kv -> $j, $u { } }
my $j = 42;
f();
say $j;    # raku: 42;  mutsu: 2
```

Same root cause as the `@`/`%` case below, and the same real fix (make the bind
a per-iteration declaration). The cross-thread-lane symptom of this family was
split off and fixed separately in `t/for-multi-param-shared-lane.t`.

```raku
my @arr = (100, 200);
for 1, [10,20], 2, [30,40] -> $a, @arr { }
say @arr;   # raku: [100 200]
            # mutsu: [30 40]
```

Sibling bug to `for-multi-param-shadow-clobbers-outer-lexical.md` (fixed), which
covered scalar (`$v`) and sigilless (`\value`) multi-param loop variables that
shadow an enclosing lexical of the same name. This is the `@`/`%`-sigil case,
deliberately left out of that fix because the root cause — and therefore the
fix — is different.

## Why the scalar fix does not apply here

`build_for_bind_stmts` binds every multi-param loop variable (regardless of
sigil) via a plain `Stmt::Assign`, not a `my`-style declaration, so it never
gets a fresh shadow slot: it reuses whatever local slot the name already
occupies (an enclosing `my @arr`) and writes into it, once per iteration, for
the loop's whole duration.

For a **scalar** name, that slot holds a plain `Value` (a `NanBox`, `Copy`-like
— see `Compiler::is_plain_lexical_name` / `plain_locals`), and `SetLocal`
replaces it wholesale. Snapshotting `locals[slot].clone()` before the loop and
writing it back after works perfectly: the clone is a distinct, cheap value
that the loop's overwrites never touch.

For an `@`/`%`-sigil name, the slot instead holds an Array/Hash **container**
(effectively a reference-counted handle whose *contents* get mutated in
place — this is exactly what lets a genuine `@arr = (...)` re-assignment to an
already-declared array preserve identity for other aliases/bindings of the
same container). Because the loop parameter reuses the very same slot as the
outer `@arr` — not a fresh container — the per-iteration bind mutates the
SAME container object the pre-loop snapshot aliases. `locals[slot].clone()`
only clones the handle (cheap, shares the backing storage), so the "restore"
in `exec_for_loop_body` (`src/vm/vm_for_loop_body.rs`) writes back a handle to
a container whose contents were already overwritten to the last iteration's
value. This was confirmed with `rust-gdb`: breaking on the restore write showed
the snapshot's `Value` bit pattern (the container handle) was already
identical to the value about to be written *before* the loop even started
running its restore code — i.e. the same object, contents just mutated.

## Where it is

`src/vm/vm_for_loop_body.rs`, the multi-param restore loop (search for
`saved_multi_params`). The restore explicitly excludes `@`/`%`/`&`-sigil names
from the local-slot write-through:

```rust
if !name.starts_with(['@', '%', '&'])
    && let Some((slot, v)) = saved_local
{
    self.locals[slot] = v;
}
```

Naively removing that exclusion does NOT fix the array/hash case (verified) —
it just performs a no-op restore of an already-mutated container.

## What a real fix needs

Simply restoring a snapshotted `Value` is not enough for a container type; the
fix needs to either:

- Deep-copy the array/hash's *contents* before the loop and restore the
  contents (not just the container handle) afterward, or
- Give the multi-param bind a genuinely fresh container each iteration
  (matching Raku semantics: a plain, non-`is rw` `@arr` loop parameter is its
  own lexical, not an alias of the outer `@arr`) instead of reusing the outer
  slot's container in place — this is the same "make the multi-param bind a
  real per-iteration declaration" architectural fix floated in the sibling
  ticket, and would likely fix both problems (and the type-constraint /
  cross-thread-lane issues already noted there) uniformly.

## Why it matters

Same family as the scalar version: `for %h.kv -> $k, @results { ... }` inside
a routine that already has an outer `@results` silently corrupts it. Less
common than the scalar shadowing case (needs a same-named `@`/`%` multi-param
AND an enclosing same-named `@`/`%`), but the failure mode is the same —
silent data corruption, no error.

Pin when fixed: extend `t/for-multi-param-type-constraint.t` (see the
value-restore cases added for the scalar/sigilless fix) with `@`/`%`-sigil
cases.
