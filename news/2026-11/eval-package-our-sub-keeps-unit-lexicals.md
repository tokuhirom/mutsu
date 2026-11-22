# An EVAL'd package keeps its subs' file-scope lexicals and exports

```raku
use MONKEY;
module QQ { EVAL q[my $pcs = 5; our sub f { $pcs }] }; say QQ::f();
EVAL q[unit module M; sub g is export { 7 }]; say M::EXPORT::DEFAULT::<&g>();
```

mutsu printed `(Any)` and then died with "No such method 'CALL-ME' for
invocant of type 'Nil'"; rakudo prints `5` and `7` (#17944).

An EVAL unit's named subs capture the unit's file-scope `my` variables in a
bucket of their own, which outlives the EVAL. That bucket was only given at
`GLOBAL`. An EVAL run inside a package, or one opening its own with
`unit module`, now gets it too. The package routine reads it ahead of the
package's own block lexicals, so a same-named `my` of the enclosing package
does not answer for the unit's. A sub called through its code object now
records its body on the routine frame. That keeps two EVAL units declaring a
same-named sub in different packages reading their own variables.

The `EXPORT::<tag>::name` aliases an EVAL unit installs are symbols of the
package's EXPORT stash. The unit's two registry rollbacks now keep them, while
the exported sub stays out of the caller's lexical scope.
