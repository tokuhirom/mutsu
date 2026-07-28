# A hyper method call on an itemized list does not descend into it

```raku
my $i = $(:a(1), :b(2), :c(3));
say $i.^name;        # List           (both)
say ($i>>.key).raku; # raku: ("a", "b", "c")
                     # mutsu: ($("a", "b", "c"),)
say $i>>.key.sort;   # raku: (a b c)
                     # mutsu: (((a b c)))
```

mutsu treats the itemized list as **one** element and maps over that single
element, so the result is a one-element list wrapping the original. Every
downstream operation then sees one element: `.sort` above is a no-op on a
one-element list, which is how the wrong ordering reaches the caller.

`>>.` is `deepmap`-shaped: it descends into `Iterable` nodes. A `List` in a
`Scalar` container is still a `List`, and Rakudo descends into it. mutsu's
per-element descend rule (`itemize_if_descended`, keyed off whether the *source
element* is `Iterable`) is right; the bug is one level up, in how the hyper op
enumerates the target's elements in the first place.

Both hyper method entry points take the target's elements with
`crate::runtime::value_to_list(&target)`
(`src/vm/vm_hyper_method_ops.rs:357` and `:1036`), and that is **list-assignment
flattening**, under which an itemized list is deliberately one element:

```raku
my $i = $(:a(1), :b(2));
say $i.elems;             # 2   (both)
say (my @x = $i).elems;   # 1   (both -- correct)
```

So `value_to_list` is doing exactly what it is for, and is simply the wrong
question here: a hyper wants the node's *own* elements, not what the node
contributes to a flattening list assignment. The fix is one decision point —
enumerate the target's elements directly when it is a `List`/`Array`, itemized
or not — not a change to `itemize_if_descended`.

## Where it bites

It is the last failing assertion in `DBIish`'s `t/01-basic.rakutest`, which is
otherwise at raku parity (34 of 35) now that
[ADR-0015 P2](../../news/2026-07/buf-repr-body-and-native-storage.md) and the
[ternary/enum-value parse fix](../../news/2026-07/ternary-then-branch-enum-value.md)
have landed:

```raku
my $installed = DBIish.installed-drivers;   # an itemized List of Pairs
is $installed>>.key.sort, @drivers, 'The expected five';
# expected: 'Oracle Pg SQLite TestMock mysql'
#      got: 'Oracle Pg TestMock mysql SQLite'
```

`DBIish` is the database battery (PLAN.md §1), so this is what stands between it
and 9/9 files at raku parity.

## Worth checking while there

Whether the same one-element wrapping happens for the other hyper forms
(`>>.method(args)`, `<<op>>`, `.map` under `hyper`) on an itemized source, and
whether a *nested* itemized list (`$($(1,2), $(3,4))`) descends to the right
depth. The rule to match is Rakudo's `nodal`/`deepmap` behaviour, so the fix
should be one decision point, not a special case per form.
