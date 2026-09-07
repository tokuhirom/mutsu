# An array assignment in VALUE position does not decompose an `is Array` subclass

Split out of `news/2026-09/array-subclass-iterator-override.md` (2026-09-07),
which fixed the statement-position spelling. Everything below is what is left.

## Repro

```raku
class SA is Array { }
my @a := SA.new(3,2,1,4);

my @c = @a;              # statement position
say @c.raku;             # raku: [3, 2, 1, 4]     mutsu: [3, 2, 1, 4]   -- correct

say (my @b = @a).raku;   # value position
                         # raku: [3, 2, 1, 4]     mutsu: [[3, 2, 1, 4]]
```

The same assignment, written where its result is consumed, still stores the
instance as a single element. With an `iterator` override on the class the
value-position form is wrong in the same way (`[[1, 2, 3, 4]]` where rakudo
says `[1, 2, 3, 4]`), so this is one gap, not two.

## Where to look

The statement-position fix landed in `Interpreter::set_local_*`
(`src/vm/vm_var_assign_set_local.rs`), on the `else if` chain that already
handled a `does Iterable` instance through `try_iterable_instance_items`: an
`is Array`/`is List` subclass instance now distributes its
`__mutsu_array_storage` elements there. The parenthesised form evidently
compiles to a different assignment path that never reaches that chain — find
it and give it the same rule (ideally by routing both through one helper, so
the next such divergence cannot happen).

## Check when fixing

`say (my @b = @a).raku` and `say (@b = @a).raku` for an already-declared `@b`;
the same two with an `iterator` override (which must follow the override, as
the statement form does); `my $c = SA.new(...)` in value position, which must
STAY one element (`[[3, 2, 1, 4],]` in rakudo — an itemized scalar is not
decomposed); and `t/array-subclass-iterator-override.t`, which pins the
statement-position behaviour.
