# Curried-role puns, `is R[...]` containers, `$.WHICH` accessors, AT-POS exceptions

Found by the ecosystem roulette on Rake:

- `R[Int,Str].^pun` is now the very class `R[Int,Str].new` constructs
  through, so an instance's `.WHAT =:=` its pun (it used to return a separate
  `(R[Int,Str])` type).
- `my @a is R[Int,Str] = ...` and `constant RC = R[Int,Str]; my @a is RC = ...`
  tie the variable to that pun, as `my @a is SomeClass` does.
- A named role argument is not part of a curried role's type identity:
  an instance of `class B does R[Int,Str,:v]` satisfies `R[Int,Str]`.
- A public attribute named `WHICH` (`has ObjAt $.WHICH`) overrides `Mu.WHICH`,
  including for Set membership, and `self.Mu::WHICH` reaches the object's own
  identity.
- An exception thrown by a class's own `AT-POS` propagates from `$obj[$i]`
  instead of becoming Nil (so `dies-ok`/`throws-like` see it, and a CATCH
  around the read no longer resumes).

Rake's `t/01-basic.rakutest` is down to the two tests that need #11261 (a
block-scoped `constant` shadowing a later block's same-named `my class`).
