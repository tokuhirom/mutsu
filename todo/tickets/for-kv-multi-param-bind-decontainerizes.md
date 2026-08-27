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

## Reproduce

The three snippets above, no fixtures.
