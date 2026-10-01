# A nested BEGIN that auto-declares under `no strict` is lifted

```raku
sub n { no strict; BEGIN { $auto = 3; say "auto $auto" } }; say "m"
```

rakudo prints `auto 3` and then `m`: the BEGIN runs at BEGIN time though `n` is
never called. mutsu printed only `m`, because the ADR-0134 walker did not lift a
BEGIN whose body names a plain lexical the unit never declares.

The walker refuses such a name for a reason: in an EVAL it may be a lexical of
the EVAL's *caller*, which the prologue cannot supply. Outside an EVAL, with
`strict` off where the BEGIN sits, it can only be an auto-declared package
variable, which the lifted block (it repeats `no strict`) declares the same
way. Two facts are therefore needed and are now passed in:

- whether the unit is an EVAL's. `reorder_phasers_for_eval` says so; the
  mainline and module entry points do not. It travels through `reorder_recursive`
  and `take_unit_prologue` to the walker as part of a small `UnitContext`.
- whether `strict` is off where the BEGIN sits: the last `use strict` /
  `no strict` among the enclosing scopes' pragmas (`Frame::imports`), and then
  among the unit's top-level statements ahead of the statement being walked.

Lifting these BEGINs exposed a limit: a lifted body runs in a block of its own,
and mutsu drops the bare-name binding of a variable auto-declared in a block
when the block exits (`no strict; { $a = 5 }; say $a` is `(Any)` here and `5` in
rakudo, though `$GLOBAL::a` is set). The BEGIN, left in place, ran in its
routine's own scope and set the variable there, so `sub f { no strict; BEGIN {
$c = 1 }; $c }` answered `1`; lifted it answered `(Any)`. To avoid trading one
wrong answer for another, a name the unit mentions anywhere outside its BEGIN
bodies is not lifted (an `ast_visit::Visit` scan of the unit, once per unit).
Only a name the BEGINs alone use is, which is exactly the reported shape; two
BEGINs may share one.

That is deliberately conservative, and it has a consequence the ADR already
documents: a BEGIN that is not lifted keeps the later BEGINs of its unit from
being lifted too, so source order holds.

Pinned by `t/control/begin-prologue-no-strict-auto-declared.t` (6 tests,
rakudo's output): a routine, a nested block, an array, a hash, two BEGINs sharing
a name, a file-scope `no strict`, and the shared-name case that stays put.

Not changed here: `use strict` (or a `no strict` followed by `use strict`) over
the same body is a compile-time `X::Undeclared` in rakudo and runs silently in
mutsu (the BEGIN is not lifted either way). That is a gap in mutsu's strict
check of an uncalled routine's body, not in the lift.
