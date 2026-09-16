# A plain class's `.iterator` override is now honored by map/grep/sort/first/head/tail

A class that defines its own `iterator` method, but does not `does Iterable`,
was treated by `.map`, `.grep`, `.sort`, `.first`, `.head` and `.tail` as a
single opaque element instead of decomposing through the override:

```raku
class Wrapper {
    has @.items;
    method iterator { @.items.iterator }
}
my $w = Wrapper.new(items => [1, 2, 3, 4, 5]);
say $w.map({ $_ * 2 }).elems;   # raku: 5, mutsu (before): 1
```

The root cause: `value_to_list`/`value_to_list_for_receiver`
(`src/runtime/utils/list.rs`) are pure `Value -> Vec<Value>` functions with no
interpreter access, so they cannot dispatch a user-defined `.iterator`
method. `call_method_with_values` already had an "Any iteration methods" arm
that drove a `does Iterable` class's own iterator correctly, but it reused
`try_iterable_instance_items`, which additionally requires
`class_does_role(cn, "Iterable")` — a plain class merely defining `iterator`
does not satisfy that.

Measured against `raku`: a plain class's override is honored by
`map`/`grep`/`sort`/`first`/`head`/`tail` exactly like a `does Iterable`
class, but NOT by `flat`/`list`/`elems`/`kv`/`pairs`/`raku`/`gist`, and not by
`for`/`@`-assignment either. So the fix adds a narrower
`try_user_iterator_items` helper (excluding `flat`) for that arm, rather than
relaxing `try_iterable_instance_items` itself — `for`, `@`-assignment and
`vm_data_ops.rs` still need its stricter, Iterable-role-gated behavior, and
relaxing it directly would have made those wrongly decompose too.

The native fast-path decline check in `vm_native_dispatch.rs` (which steps
aside so the slow-path arm can run) is relaxed the same way, dropping its
`Iterable`-role requirement.

This was the root cause of `Game::Entities` 0.1.6's `t/entities.t` failures
(`View`'s `method iterator { $!view.iterator }` has no `does Iterable`/
`Positional`), reduced in issue #8547.

Regression test: `t/collections/lazy-seq/instance-iterator-override.t`.
