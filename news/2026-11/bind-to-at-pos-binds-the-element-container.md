# Binding to `@a.AT-POS($i)` binds the element's container

`my $node := @a.AT-POS($i)` is the subscript `@a[$i]` spelled as a method, and rakudo binds the
element's container either way: a missing element is autovivified when the binding is first
assigned to. mutsu only gave the subscript spelling the bound-declaration route. The method form
bound the plain `Any` read, so `$node = value` died with "assign requires a concrete object (got
a Any type object instead)".

The binding-declaration expansion now rewrites a one-argument `AT-POS` call on an `@` variable
to the subscript before choosing its route. The rewrite is the one call arguments already got
(`f(@a.AT-POS($i))`).

BinaryHeap 2.1.2 inserts this way (`my $node := @!array.AT-POS($!elems); $node = value`), and its
`t/01-binaryheap` and `t/02-autovivify` died on it. Every file of its suite now matches raku; the
only remaining difference is the order of `throws-like`'s named checks, which raku itself varies
from run to run. `t/collections/bind-at-pos-element-container.t` pins the behaviour.
