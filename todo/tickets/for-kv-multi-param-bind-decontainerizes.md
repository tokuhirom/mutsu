# A multi-parameter `for` bind decontainerizes, so `.kv` cannot hand out element containers

## Symptom

`for @a.kv -> $i, $v is rw { ... }` propagates a **direct** write but loses a
**deferred** one, where the single-parameter forms now do both:

```raku
my @a = 10, 20;
for @a.kv -> $i, $v is rw { $v += $i }
say @a;                        # [10 21]  -- correct, via the writeback

my @b = 10, 20; my @c;
for @b.kv -> $i, $v is rw { @c.push(-> { $v = $v + 1 }) }
@c[0](); @c[1]();
say @b;                        # raku [11 21]   mutsu [10 20]
```

The hash twin diverges the same way:

```raku
my %h = a => 1, b => 2; my @c;
for %h.kv -> $k, $v is rw { @c.push(-> { $v = $v + 1 }) }
@c[0](); @c[1]();
say %h.sort;                   # raku (a => 2 b => 3)   mutsu (a => 1 b => 2)
```

These are ADR-0045 §1.3 row 16 and its hash sibling.

## Root cause

ADR-0036 slice 3 / ADR-0045 slice 4 added a container-aware producer layer
(`src/vm/vm_element_producers.rs`) so `.pairs`/`.values`/`.reverse`/`.sort` hand
out the elements' own `Scalar` containers. `.kv` was **excluded**, and the
exclusion is about the *consumer*, not the producer:

A `.kv` loop is a **multi-parameter** loop, and a multi-parameter loop does not
bind at the native bind site the other forms use (`exec_for_loop_body` in
`src/vm/vm_for_loop_body.rs`). It binds through bind-prefix `Stmt::Assign`s that
`build_for_bind_stmts` (`src/compiler/mod.rs`, ~line 3069-3121) emits, each
reading its chunk element. That read goes through the ordinary element
chokepoint, which **decontainerizes** — so a cell handed out by the producer
arrives at `$v` as a plain value, while the writeback that used to carry the
mutation has been retired for that iteration precisely *because* the chunk
carried a cell. The result is a silent loss, which is why `.kv` stays on the
snapshot producer for now.

## What the fix needs

A **raw (non-decontainerizing) bind for an rw scalar multi-parameter**. The
shape already exists for the container sigils: `build_for_bind_stmts` wraps an
`@`/`%`-sigil multi-param in `Stmt::SyntheticBlock([Stmt::MarkBind, decl])`, the
same marker `my @a := expr` uses, exactly so the bind does not coerce. Extending
that to a scalar parameter that is `is rw` / `<->` / sigilless is the natural
route, but it is a change to the bind-prefix machinery that every multi-parameter
loop in the corpus goes through, so it wants its own verification pass rather
than riding along with the producer routing.

Once it lands, add `"kv"` back to `ELEMENT_PRODUCERS` in
`src/vm/vm_element_producers.rs` (both the array and hash arms) and un-`todo`
row 16 in `t/for-loop-element-alias.t`.

## Also blocked on the same thing

`(@a.kv)[1].VAR.^name` reports `Str` where raku reports `Scalar`, for the same
reason: the `.kv` list holds plain values. (The neighbouring `(@a[0]:kv)[1]`
form is already `todo`-marked in `t/subscript-pair-element-container.t` for a
*different* reason — `.VAR` on an anonymous computed index target, see
`todo/tickets/var-on-a-containerref-is-not-distinguishable.md`.)

## Measured 2026-09-01: the prescribed fix works, and what it uncovered

The "raw bind for an rw scalar multi-parameter" above was implemented and
measured. It is the right fix and it works:

* `build_for_bind_stmts` (`src/compiler/mod.rs`) gains `rw_block: bool`, and a
  multi-parameter that is rw (`<->`, `is rw`, or sigilless), positional, with no
  default, emits `Stmt::SyntheticBlock([Stmt::MarkBind, decl])` instead of
  `Stmt::Assign` -- the same shape the `@`/`%` case already uses.
* `array_slot_ref` is idempotent (`value_methods_b.rs:258-260` returns the cell
  the slot already holds rather than wrapping it), so `my $v := @_[1]` over a
  chunk that carries a source cell aliases the **source** element, not the
  temporary chunk. That is what makes the routing work at all.
* With that alone -- *before* adding `"kv"` to `ELEMENT_PRODUCERS` -- both
  deferred-closure snippets above already produce raku's answer.

**But it broke seven unrelated, already-green ADR-0045 rows** (01, 02, 03, 12,
13, 14, 08), and the cause was not the routing. The new `my $v := ...` puts a
slot named `v` in the enclosing frame's flat local map, and *any* same-named
`my` did that. Reduced:

```raku
{ my @a = 10, 20; my @c;
  for @a -> $v is rw { @c.push(-> { $v = $v + 1 }) }
  @c[0](); @c[1](); say @a }    # raku [11 21]   mutsu [10 20]
{ my $v = 1; say $v }           # <- deleting this block made the above pass
```

That reproduced on a clean `main` with no `.kv` involved, and is fixed
separately -- `news/2026-09/closure-capture-slot-name-search-finds-a-later-my.md`
(`resolve_capture_slot`'s name search over the creating frame's local slots
found a slot declared LATER in the same compilation unit). **This ticket is
unblocked by that fix**; re-apply the compiler change above, add `"kv"` to
`ELEMENT_PRODUCERS` (both the array and hash arms -- the array arm must yield a
flat `index, cell, index, cell, ...` and the hash arm `key, cell, ...`, since
the loop chunks the flat list by 2), and un-`todo` row 16.

Note also that `(@a.kv)[1].VAR.^name` reports `Int`, not the `Str` this ticket
records.

## Reproduce

The three snippets above, no fixtures.
