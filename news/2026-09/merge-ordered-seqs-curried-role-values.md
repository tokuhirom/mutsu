# MergeOrderedSeqs passes: curried-role value arguments and iterators held in arrays

The `MergeOrderedSeqs` distribution (0.0.2) now passes its test suite: `t/01-basic.rakutest` goes
from 1/2 (it died at the second assertion) to 19/19, the same as rakudo. The distribution is a
parameterized iterator role, `role MergeOrderedSeqs[$before = Less] does Iterator`, and it exposed
four general gaps:

- **`R[|()]` is `R`.** A Slip in a role's argument list now spreads into it, so an empty one passes
  no arguments and the parameter defaults apply. Before, the empty Slip was bound to `$before` as a
  single argument. The distribution builds `MergeOrderedSeqs[|($_ with $before)]`, and the attribute
  default derived from `$before` was lost. `R[|@a]` now spreads too.
- **`R[More]` binds `Order::More`.** The positional-subscript rule that numifies an enum index
  (`@a[Green]` is `@a[1]`) also fired on a role type object, so the role parameter got `1`. The rule
  now skips a type-object target, where `[...]` is a parameterization. The same applies to `R[True]`.
- **Distinct closures pun distinct classes.** `R[{ 1 }].new` and `R[{ 2 }].new` used to share one
  cached pun class, so the second call ran the first block. The pun cache is now keyed by the
  argument's identity when its spelling is not unique. `value_which_key` also gives a code object its
  id-based `.WHICH` (`Block|16`) instead of rendering every Block alike.
- **An iterator held in an array advances.** `@!iterators[$i].pull-one` read the cursor but never
  committed the advanced position, so an iterator stored in an array replayed its first element
  forever. The non-variable receiver path now commits the cursor through the instance's shared
  attribute cell, so every alias of the iterator sees the step.
