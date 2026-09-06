# An `is Array` subclass's `iterator` override is ignored, and the object iterates as one item

Found by the 2026-09-06 doc-diff sweep (`raku-doc/doc/Language/structures.rakudoc:123`),
re-verified against raku v2026.07 the same day.

## Repro

```raku
class SortedArray is Array {
    method iterator() { self.sort.iterator }
}
my @thing := SortedArray.new([3,2,1,4]);
.say for @thing;
# raku:  1 2 3 4  (four lines)
# mutsu: [3 2 1 4]  (one line)
```

Two things are wrong and they may be one cause or two:

1. the user's `iterator` override is not consulted, and
2. the object is iterated as a **single item** rather than as its four
   elements — mutsu prints the whole array on one line, so `for` never
   decomposed it at all.

The second is the more visible half: even without an `iterator` override, `for`
over an `is Array` subclass instance should yield its elements.

## Why it is not obviously small

`delegates_to_array_storage` (`src/vm/vm_call_method_ops.rs`) already routes an
`is Array`/`is List` subclass instance's non-user methods to its backing
`__mutsu_array_storage`, with `is_type_identity_method` as the documented
exception list. `iterator` is not in that exception list, so a *plain* `.iterator`
call on such an instance probably already reaches the storage — which would
answer the storage's iterator and ignore the override, matching what we see.
But `for` may not go through method dispatch for its iterable at all: it has its
own iterable-shape analysis, and that is the likelier reason the instance is
treated as one item.

So the fix has to decide, for an `is Array` subclass:

- does `for` ask the object for an `iterator` (raku: yes, via the `Iterable`
  protocol), and
- when a user method of that name exists, does it win over the storage
  delegation (raku: yes — a user method always wins).

Both answers look like "yes", but the second one interacts with
`delegates_to_array_storage`'s default-delegate design, which is deliberately
the opposite of the Associative side's curated allowlist. Read that function's
doc comment before changing it.

## Neighbourhood to check when fixing

`is List` as well as `is Array`; an `is Hash` subclass overriding `iterator`;
overriding `AT-POS`/`elems`/`list`/`List`/`Seq` instead; `for`, `map`, `grep`,
`.eager`, `|@thing` flattening, `@thing[0]` and `@thing.elems` over the same
object; and `my @a := SortedArray.new(...)` vs `my $a = SortedArray.new(...)`
(the repro uses `:=`).
