# A raw-invocant method over an immutable `List` element succeeds silently

Found on 2026-09-06 while measuring the neighbourhood of ADR-0067's
subscript-receiver producer. It is *not* caused by that slice — the producer
declines `List`/`ItemList` receivers on purpose, so this is the unchanged
behaviour from before it.

## Repro

```raku
use v6.e.PREVIEW; use MONKEY-TYPING;
augment class Int { method mut(\S:) { S = 7 } }

my $l = (1, 2);
$l[0].mut;          # raku: dies, "Cannot modify an immutable Int (1)"
                    # mutsu: succeeds, does nothing
say $l.raku;        # both: $(1, 2)
```

`$(1, 2)` (an `ItemList`) behaves the same way.

## What is already correct

Every *ordinary* write to an immutable list element is refused correctly, which
is what makes this narrow:

| # | Program | raku | mutsu |
|---|---|---|---|
| A1 | `my $l = (1,2); $l[0].mut` (raw invocant) | dies | **lives, no-op** |
| A2 | `my $m = (1,2); $m[0] = 7` | dies | dies |
| A3 | `my @n := (1,2); @n[0] = 7` | dies | dies |
| A4 | `(1,2)[0] = 7` | dies | dies |
| A5 | `my $s = $(1,2); $s[0].mut` (raw invocant) | dies | **lives, no-op** |

So the store path enforces immutability; the *method-call* path does not consult
it at all.

## Family

This is the same gap ADR-0067 records as rows L4/L5/J5 and M1/M2 — a body that
writes through an invocant it was not given a location for. raku refuses at the
binder ("Cannot modify an immutable value" / "Cannot assign to a readonly
variable or a value"); mutsu binds the invocant by value and drops the write.
The *observable* half already matches (the caller's data is not modified), which
is why none of these are silent wrong answers — but the missing diagnostic means
a program that relies on the refusal runs on.

## Why it is a ticket rather than part of that slice

Fixing it means enforcing readonly-ness **at the parameter binder**, for every
raw-invocant parameter bound to a value that has no location — not at any one
producer. That touches both binders (`call_compiled_method` and
`call_compiled_method_fast`), needs the "was this invocant a location?" fact to
survive to the bind, and has to produce raku's two distinct diagnostics. It also
has to leave `M1`/`M2` (`42.mut`, `($a + 1).mut`) consistent, since they are the
same question asked of an rvalue.

## Acceptance

A1 and A5 die with raku's message; A2-A4 keep dying as they do today; the
ADR-0067 pins (`t/raw-invocant-arrives-as-container.t`,
`t/raw-invocant-subscript-receiver.t`) stay green, including their L4/L5 rows,
which assert only that the caller's variable is unmodified and would have to be
tightened to assert the refusal as part of this work.
