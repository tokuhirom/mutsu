# A slice assignment with a too-short RHS broadcasts the last value instead of padding

Found 2026-09-07 while fixing
`news/2026-09/itemized-hash-subscript-is-one-key.md`; pre-existing and
independent of that change, so it was left out of scope there.

A slice assignment is a **list** assignment: raku zips the RHS list against the
targeted slots and the slots that run past the end of the RHS get the
container's undefined value. mutsu instead re-uses the RHS for every slot, so a
one-element RHS fills the whole slice.

## Repro

```raku
my %h; %h{(1, 2)} = "z";        say %h.raku;
# raku:  {"1" => "z", "2" => Any}
# mutsu: {"1" => "z", "2" => "z"}

my %i; %i<a b> = 1;             say %i.raku;
# raku:  {:a(1), :b(Any)}
# mutsu: {:a(1), :b(1)}

my %j; %j{(1, 2, 3)} = "z", "y"; say %j.raku;
# raku:  {"1" => "z", "2" => "y", "3" => Any}
# mutsu: {"1" => "z", "2" => "y", "3" => "z"}   # last RHS value repeats
```

So it is not "a single scalar broadcasts" — with a two-element RHS across three
slots the *last* element repeats. The zip simply never terminates; it clamps
its RHS index instead of running out.

## The positional twin is close but uses the wrong filler

```raku
my @a; @a[0, 1, 2] = "z",; say @a.raku;
# raku:  ["z", Any, Any]
# mutsu: ["z", Nil, Nil]
```

The positional path already stops instead of broadcasting, so only the padding
value is wrong there: raku pads with the element type's undefined value (`Any`
for an untyped `Array`), mutsu pads with `Nil`. Both halves want the same
notion of "the slot's unassigned value", which is what
`Interpreter::unassigned_lexical_value` computes for a declaration.

## Where to look

`src/vm/vm_var_assign_index_named.rs`, the slice arm
(`ValueView::Array(keys, kind) if is_positional || !kind.is_itemized()`), which
pairs `keys` with `self.assignment_rhs_values(&val)?`. The associative and
positional halves diverge from raku differently, so check both; a typed target
(`my Int %h`, `my Int @a`) must pad with that type's undefined value rather
than a bare `Any`.

## Check when fixing

An RHS longer than the slice (the extra values are dropped, no error); an empty
RHS; a `Seq` RHS, which ADR-0058 already makes eager for a slice assignment; a
typed hash and a typed array; and `roast/S32-hash/adverbs.t` plus
`roast/S09-subscript/slice.t`, which exercise these shapes.
