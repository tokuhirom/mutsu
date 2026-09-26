# Set operators take Any, Nil and "" as elements; `op=` seeds `set()` / `bag()`

The set operators and `.Set` / `.Bag` silently dropped an operand or element
that was the `Any` type object, `Nil`, or stringified to `""`
(`Any ∪ set(1)` was `Set.new(1)`; Rakudo gives `Set.new(1, Any)`). The rule
existed so that `my $s; $s ∪= 0` starts from the empty set (roast
`S04-phasers/enter-leave.t`). In Rakudo that result comes from
`METAOP_ASSIGN`: an undefined left side of `op=` is replaced by the operator's
zero-argument value, and the operator itself drops nothing (#9481).

mutsu now works the same way. The set compound assignments (`∪=`, `∩=`, `(-)=`,
`(^)=`, `(+)=`, `(.)=` and their ASCII spellings) wrap their left side in the
same `MetaAssignIdentity` seed `+=` and `~=` use, with two new identities:
`EmptySet` and `EmptyBag`. The exclusions in `set_operand.rs` and
`set_coerce.rs` are gone.

That also fixes a typed container. `my Set $s; $s ∪= 3` gave
`Set.new(3, Set)`, because the type object became an element. It now gives
`Set.new(3)`.

Test: `t/collections/set-bag-mix/set-ops-undefined-operands-are-elements.t`.
