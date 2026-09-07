# A slice assignment pads a short RHS instead of broadcasting it

A slice assignment is a **list** assignment: raku zips the RHS list against the
targeted slots, and the slots that run past the end of the RHS get the
container's undefined value. mutsu diverged on both halves, differently.

```raku
my %h; %h{(1, 2)} = "z";         # raku: {"1" => "z", "2" => Any}
                                 # was:  {"1" => "z", "2" => "z"}
my %j; %j{(1, 2, 3)} = "z", "y"; # raku: {"1" => "z", "2" => "y", "3" => Any}
                                 # was:  {... "3" => "z"}   -- the LAST value repeated
my @a; @a[0, 1, 2] = "z",;       # raku: ["z", Any, Any]
                                 # was:  ["z", Nil, Nil]
```

The associative half indexed its RHS with `vals[i % vals.len()]`, so the zip
never terminated — it wrapped. That is why it was not simply "a single scalar
broadcasts": with a two-element RHS across three slots the *last* element
repeated.

The positional half already stopped rather than wrapping, so only the filler
was wrong — and `Nil` is not even the hole value mutsu's own arrays render
(`my @a; @a[2] = 1` is `[Any, Any, 1]`), so the two disagreed with each other
as well as with raku.

## The fix

One notion of "the slot's unassigned value" for both halves:
`Interpreter::slice_pad_value` reads the target's declared element constraint
and hands it to `typed_scalar_nil_seed_value` — the same function a declaration
uses — falling back to `Any` for an untyped container. So a typed target pads
with its own type object (`my Int @a; @a[0,1] = 1` leaves `Int` at index 1,
`my Int %h; %h<a b> = 1` leaves `Int` at `b`), and a native element type pads
with the value it actually has rather than a type object it does not:
`array[int]` with `0`, `array[str]` with `""`, `array[num]` with `0e0`. All
measured against raku.

It is applied at the three slice sites that zip a RHS against keys — the
associative slice, the flat positional slice, and the 1-D shaped-array slice.
The `SetHash`/`BagHash`/`MixHash` slice deliberately keeps `Nil`: a Set slot's
value is its Bool membership and a Bag/Mix slot's is its count, where raku's own
answer is `Nil` → 0 with a "Use of Nil in numeric context" warning.

## The hyper spelling is the opposite rule, and keeps it

`%h<a b c> »=» 7` **cycles** the RHS across the targets — that is the whole
point of the metaoperator, and it is not just scalar broadcast:
`%h<a b c> »=» (1, 2)` is `a => 1, b => 2, c => 1`. The old
`vals[i % vals.len()]` was in fact hyper semantics applied to *every* slice
assignment, so simply removing it broke `roast/S13-overloading/metaoperators.t`
and `t/index-assign-shadow-slot.t` (caught by the local `make roast`).

The two spellings are the same `IndexAssign` node by the time the VM sees them
— the hyper desugaring lowers to a plain subscript assignment against a temp —
so the desugaring now says which it is, with a `Stmt::MarkHyperSliceAssign`
marker ahead of the assignment (the same one-shot-flag shape as the existing
`Stmt::MarkBind` / `OpCode::MarkBindContext`). `slice_rhs_value` reads it and
cycles instead of padding; the flag is consumed whether or not the assignment
turns out to be a slice, so it cannot leak onto the next one.

The neighbourhood the ticket listed is unmoved: an RHS longer than the slice
still drops the extra values with no error, an empty RHS pads every slot, a
`Seq` RHS is still eager (ADR-0058) and pads its short tail, and an object hash
(`my %h{Any}`) was already right.

Pinned by `t/slice-assign-pads-short-rhs.t`, whose 20 assertions — the four
hyper rows included — pass unchanged under rakudo.
