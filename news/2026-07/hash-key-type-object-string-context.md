# Type-object hash keys coerce to "" with a string-context warning

A bare type object used as a plain (`Str`-keyed) hash subscript key now coerces
to the empty string with Rakudo's "Use of uninitialized value of type X in
string context" warning, instead of keying by its `(Int)`-style gist.

```raku
my %h;
%h{Int} = 1;        # warns; keys under ""
say %h.keys.raku;   # ("",).Seq   (was ("(Int)",).Seq)
say %h{""};         # 1           — the same entry
```

Because every distinct type object stringifies to the same `""`, they collapse
onto one key (`%h{Int} = 1; %h{Str} = 2` leaves a single entry holding `2`),
matching Rakudo. The coercion is applied uniformly across the whole subscript
family — store, read, `:exists` and `:delete` — so all four agree on the `""`
key (previously a store keyed `(Int)` while a fresh read would have keyed the
same, but the fix keeps them consistent under the new `""` key). A type object
whose class defines a user `.Str`/`.Stringy` dispatches it instead (no warning),
and an object hash (`my %h{Any}`) keeps the type object as a genuine `.WHICH`
key.

## Implementation

The coercion lives in one VM helper, `Interpreter::coerce_type_object_hash_key`
(`src/runtime/io_env.rs`), mirroring the existing prefix-`~` string-context
logic: rule out a user `.Str`/`.Stringy` (dispatch it if present), otherwise emit
the warning via `warn_type_object_string_context` and resume with `""`. Each
subscript site calls it when the key is a `ValueView::Package` (type object):

- store — `exec_index_assign_expr_named_op_inner` (`vm_var_assign_index_named.rs`),
  with the simple-hash fast path (`try_fast_hash_element_assign`) rejecting
  type-object keys so they reach the slow path;
- read — the plain-`Hash` arm of `exec_index_op_with_positional`
  (`vm_var_index_ops.rs`);
- `:exists` — both `Hash` arms of `exec_exists_index_adv_op`
  (`vm_var_exists_ops.rs`);
- `:delete` — `exec_delete_index_named_op_inner` (`vm_var_delete_ops.rs`), again
  with the fast delete path rejecting type-object keys.

Pin: `t/hash-key-type-object.t`.

## Deferred

List-to-hash construction (`my %h = (Int, 1, Str, 2)`) still keys the type
objects by their gist rather than collapsing them onto `""`. That path runs
through `exec_make_hash_op`, a non-`Result` hash-literal builder shared by `%(…)`
literals, so warning-emitting key coercion there is a wider change left for a
follow-up.
