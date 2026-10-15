# A failed grammar parse leaks no pending error (+ pin for `if`-branch sigilless reads)

Two fixes found running the MUGS::Core suite (t/02-structure-validator and
t/03-file now match rakudo):

- `sub f(\x) { if $c { my $x = 1 } else { x } }` died with "Variable '$\x' is not declared"
  until main's term-namespace rework of sigilless bindings (#12441) fixed it; the
  regression test stays as a pin.
- A `Grammar.parse` that answered with a failed Match could leave a subrule
  argument-binding error in the thread-local regex error slot, which the next
  unrelated `~~` then raised (YAMLish's failed `load-yaml` made `5 ~~ Int`
  throw "Type check failed in binding to parameter '$indent'"). A parse that
  returns instead of raising now clears the slot.
