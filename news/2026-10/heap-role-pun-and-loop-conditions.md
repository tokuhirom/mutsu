# Heap: loop parameters, loop conditions and parameterised role puns

Drawing `Heap` from the ecosystem roulette turned up five bugs. With all of
them fixed, its `t/01-test` passes 76/76 under mutsu, matching rakudo; the
file died at test 2 before.

- **Sigilless loop parameters named like a term.** In `for ... -> UInt \i`, `i`
  now reads the loop value. Before, it read the imaginary unit: the loop
  parameter has no slot of its own, so the bare word fell through to the term
  `i`.
- **Loop conditions honour a user `Bool`.** The conditions of `while`, `until`,
  `repeat` and C-style `loop` now go through the same boolean coercion as
  `if`. Before, `gather take $.pop while self` never stopped, because the
  object's own `Bool` method was not called. A Failure tested there now counts
  as handled, as it does in `if`.
- **`self.bless` on a parameterised role.** A role's own `method new` that
  calls `self.bless` now blesses the class the role puns to when called on
  `Heap[-*]`.
- **The pun from `self.bless` on a bare role.** That pun is now withdrawn
  afterwards, as `.new`'s pun is. Before, the class it left registered under
  the role's name was reused by later `Heap[...]` puns, so they ran with the
  default comparator.
- **`&!attr` in a punned role's methods.** `&!attr` now reads the attribute
  through the role marker the punned instance carries.

One related bug is filed separately as #11652: `self.bless` on a role with
defaulted parameters leaves attribute defaults that read those parameters
unset.
