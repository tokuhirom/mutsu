# Deleting from an immutable `Map` silently succeeds

raku refuses every delete on a `Map`; mutsu performs it:

```raku
my $m = Map.new("a", 1);
say $m.DELETE-KEY("a");   # raku: dies "Can not remove values from a Map".  mutsu: 1
my %h is Map = a => 1;
say %h<a>:delete;         # raku: dies the same way.                        mutsu: 1
```

Nothing on any of the delete paths consults the container's immutability: the
`:delete` opcode (`exec_delete_index_named_op_inner`,
`src/vm/vm_var_delete_ops.rs`), the name-keyed `DELETE-KEY` method arm
(`src/vm/vm_call_method_mut_ops.rs`) and the value-level one
(`src/runtime/methods_subscript_protocol.rs`) all delete unconditionally. The
immutability marker is already at hand — `HashData::declared_type` is
`Some("Map")` — and the sibling assign path already refuses correctly
(`src/vm/vm_var_assign_index_named.rs` raises the "immutable Map" error), so
this is about reusing that check on the delete side rather than inventing one.

The quanthash arms of the same `match` do refuse (`Set`/`Bag`/`Mix` return
`X::Assignment::RO`), which is why the gap is easy to miss when reading the
delete code: the immutable case looks handled.

Found while routing `:delete` on a mixin through DELETE-KEY/DELETE-POS
([news](../../news/2026-07/delete-adverb-dispatches-through-a-mixin.md)); it is
not mixin-specific and predates that change.
