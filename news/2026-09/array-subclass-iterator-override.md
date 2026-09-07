# An `is Array` subclass's `iterator` override is honoured, and the object iterates as its elements

```raku
class SortedArray is Array {
    method iterator() { self.sort.iterator }
}
my @thing := SortedArray.new([3,2,1,4]);
.say for @thing;
# raku:  1 2 3 4      mutsu was: [3 2 1 4]   (one line)
```

The single-line output was two independent bugs stacked on top of each other,
plus a third the ticket's neighbourhood list flagged.

## 1. The constructor dropped the one-argument rule

`Array.new(|c)` slurps with `+@`, so a single non-itemized Positional argument
spreads into its elements: `Array.new([3,2,1,4])` is four. `try_native_array_construct`
implemented that, but the `is Array` **subclass** path seeded
`__mutsu_array_storage` from the raw constructor arguments, so
`SortedArray.new([3,2,1,4])` was one element holding the whole array — which is
why the repro printed a single line before iteration was even reached.
`positional_new_onearg_spread` (`src/runtime/accessors_state.rs`) now applies
the same rule. It is gated on `Array` being in the MRO, because `List.new` is a
`**@` slurpy and never spreads (`SomeList.new([3,2,1,4])` stays one element,
measured).

## 2. The `iterator` override was ignored

`delegates_to_array_storage` already lets a user method of the *called* name win
over the backing storage, so a direct `.iterator` call reached the override. But
every method raku defines *in terms of* `.iterator` was still answered by the
storage. Measured against rakudo, the split is exact:

| follows the override | answers from the reified storage |
| --- | --- |
| `for`, `map`, `grep`, `list`, `Seq`, `eager`, `flat`, `raku`, `gist`, list assignment | `[0]`, `.elems`, `.join`, `.reverse`, `.head`, `.sum`, `.List`, `.Array`, `\|@thing` |

`Interpreter::consumes_receiver_iterator` names the left-hand column and
`positional_subclass_iteration_source` drives the override for it, wired into
all three delegation entries (the `CallMethod` opcode, `CallMethodMut`, and the
interpreter's `call_method_with_values`) plus
`try_iterable_instance_items`, whose `__mutsu_array_storage` early-out is now
conditional on there being no user `iterator`.

The `CallMethodMut` site returns the override's answer directly rather than
substituting the `storage` binding: that binding is also what the mut path
*writes back*, and persisting a reordered view as the instance's storage moved
`[0]`, `.join` and `|` too — which rakudo keeps on the reified elements.

## 3. The instance was one item to `|` and to list assignment

Independent of any override: `|$vec` slipped a one-item slip holding the
instance, and `my @b = @vec` stored it as a single element, where rakudo
decomposes both into the elements. `exec_make_slip_op` and the array-assignment
path in `vm_var_assign_set_local.rs` now read the backing storage (and, for the
assignment, the override when there is one — rakudo's list assignment goes
through the Iterable protocol, `|` does not).

Pinned by `t/array-subclass-iterator-override.t`, whose 25 assertions pass
unchanged under rakudo. `is List` subclasses are covered by the same tests.

One narrower gap remains and is recorded in
`todo/tickets/array-subclass-assignment-in-expression-position.md`: the same
list assignment written in **value position** (`say (my @b = @vec).raku`) still
itemizes, because that spelling does not reach the `SetLocal` path this fixed.
