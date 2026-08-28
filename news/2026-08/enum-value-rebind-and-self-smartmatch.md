# Two enum bugs: a spurious readonly error on an unrelated rebind, and a type object not matching itself

Both found while closing out `roast/S12-enums/misc.t`'s `MUTSU_REAL_TEST=1`
residue (`throws-like { Direction( 2 <=> 3 ) }, X::Enum::NoValue, type =>
Direction, value => Less`), and both turned out to be general VM
correctness bugs unrelated to `Test`/exception-object completeness at all.

## A for-loop's second `.kv` param could spuriously raise X::Assignment::RO

`for %h.kv -> $k, $v { ... }` rebinds `$v` on every iteration. When `$v` is
not compiled to a local slot (e.g. inside a nested closure/CATCH scope), the
rebind reaches `OpCode::SetGlobal`, which carries a heuristic guard against
reassigning an enum CONSTANT's own binding (`Red = 5` must die). That guard
checked only "does the variable's *current* value happen to be an enum
member" — with no check that the variable being written is actually the
constant's own name. So once a hash's (randomized) iteration order put an
enum value in one iteration, the *next* iteration's ordinary rebind of the
same loop variable inherited that enum value as its "current" content and
was misread as "reassigning the enum constant", raising a spurious
`X::Assignment::RO` — even though nothing in the loop ever touched the
constant itself.

Fixed in `src/vm/vm_exec_dispatch.rs`: the guard now also requires the
write target's name to equal the stored member's own `key` (`env["Red"] ==
Enum { key: "Red", .. }` only for a genuine `Red = ...` write), which a
merely-enum-valued unrelated variable never satisfies. Pinned by
`t/enum-value-does-not-block-unrelated-rebind.t`, green under `raku` too.

## An enum's type object did not smartmatch itself

`Int ~~ Int` is `True` — any type object smartmatches its own type. mutsu's
enum-specific smartmatch arm (`src/runtime/seq_helpers/smart_match.rs`)
only ever checked whether the LHS was an enum *value* (`Red ~~ Color`); it
never considered the LHS being the enum's own type object compared to
itself, so `Color ~~ Color` answered `False`. In turn, any `$x ~~ Color`
matcher failed whenever `$x` held the type object rather than a member —
which is exactly what `X::Enum::NoValue.type` is (the enum's type object,
not one of its values), so `throws-like …, type => Direction` always
failed its `.type` check.

Fixed by also checking the LHS-is-the-same-type-object case (respecting
`:U`/`:D` definedness smileys the same way the general Package-vs-Package
path below it already does). Pinned by
`t/enum-type-object-smartmatches-itself.t`, green under `raku` too.

Both fixes together close `roast/S12-enums/misc.t` under `MUTSU_REAL_TEST=1`
(previously aborted partway through the file with a desynchronized TAP plan;
now passes cleanly, 28/28).
