# `:delete` dispatches DELETE-KEY / DELETE-POS through a mixin

`:exists` on a mixin was fixed in
[the previous slice](exists-adverb-dispatches-through-a-mixin.md); `:delete` had
the same shape and did nothing at all:

```raku
role R { }
my %h = a => 1;
my $m = %h but R;
say $m<a>:delete;        # raku: 1 (and the key is gone).  mutsu: Nil (key stays)
say $m.DELETE-KEY('a');  # raku: 1.  mutsu: No such method 'DELETE-KEY' for
                         #               invocant of type 'Hash'
```

## Root cause

Three separate gaps, each hiding the next:

1. **The delete opcode does not recognise a `Mixin`.** `exec_delete_index_named_op`
   resolves its container out of env by variable name and then walks a chain of
   container arms; a `Mixin` matches none of them, so the delete fell through to
   the missing-container tail and returned Nil.
2. **There is no value-level `DELETE-KEY`.** mutsu implements `%h<k>:delete`
   directly in the opcode, so the method existed only on the name-keyed mut path
   (`vm_call_method_mut_ops`), which answers for a plain lexical `%h`. A hash
   reached any other way — above all the one inside a mixin's `Arc<Value>` —
   found no method.
3. **`DELETE-POS` on an array value did not mutate the array.** It rebuilt the
   array and then rewrote every *env binding* pointing at the old backing node
   (`overwrite_array_bindings_by_identity`). An array held inside a `Mixin` is
   not an env binding, so the delete was computed, returned, and lost.

## Fix

The opcode now handles a mixin the way the value it wraps deserves. Where a
composed role supplies the protocol itself — `role R { method DELETE-KEY($k) {…} }`,
or the punned `does Associative` role delegating to a private hash that motivated
the `:exists` fix — the protocol method is dispatched, exactly as it already was
for an `Instance`. Where the role supplies nothing, the delete belongs to the
wrapped container, so the opcode unwraps it into env for the duration of the op
and re-wraps the mutated container afterwards. Unwrapping rather than
reimplementing the delete for mixins keeps all of the surrounding bookkeeping —
the `__mutsu_deleted_index::` markers, trailing-hole trimming, the
container-metadata re-tag — working unchanged, because every bit of it is keyed
by the *variable name*, which the unwrap does not change.

The method form is now answered at the value level by a new
`src/runtime/methods_subscript_protocol.rs`: `DELETE-KEY` on a hash and
`DELETE-POS` on an array, both deleting *through the shared backing node* so
every holder of the container observes it — including a `Mixin`'s `Arc<Value>`,
which no env-scanning writeback can reach. `DELETE-POS` also trims trailing holes
now, so `[1,2,3].DELETE-POS(2)` leaves `[1, 2]` as it does in raku, while an
explicitly-assigned type object is not a hole and `[1, 2, Any]` keeps its length.

One more divergence surfaced on the way and is fixed here, because `:exists` on a
mixin dispatches through `EXISTS-POS` and so began to depend on it: the
value-level `EXISTS-POS` reported every in-range slot as existing, ignoring holes.
`my @a = 1,2,3; @a[1]:delete; @a.EXISTS-POS(1)` answered `True`. It consults
`ArrayData::hole_at` — the predicate `:exists`/`:k`/`:p` already share — now.

Pinned by `t/delete-adverb-on-mixin.t` (22 assertions, verified to pass under
`raku` as well as mutsu).

## Not covered

Two adjacent divergences found while doing this, both predating it and neither
mixin-specific, are recorded as tickets rather than folded in:
`todo/tickets/shaped-array-exists-reports-unassigned-slots.md` (`:exists` on a
shaped array reports unassigned slots as existing — the fix belongs in shaped
allocation, not in the `:exists` sites) and
`todo/tickets/map-delete-is-not-refused.md` (deleting from an immutable `Map`
silently succeeds).
