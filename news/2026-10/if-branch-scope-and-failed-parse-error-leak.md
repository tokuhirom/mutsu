# `if` branches scope their `my $x` shadows; a failed grammar parse leaks no error

Two fixes found running the MUGS::Core suite (t/02-structure-validator and
t/03-file now match rakudo):

- `sub f(\x) { if $c { my $x = 1 } else { x } }`: the `my $x` in the first branch
  parked the sigilless `\x` under a term slot that the *else* branch and the code
  after the `if` still read, dying with "Variable '$\x' is not declared". The
  shadow now ends with the branch block (statement modifiers, which open no
  block, keep it), for both statement and value position `if`.
- A `Grammar.parse` that answered with a failed Match could leave a subrule
  argument-binding error in the thread-local regex error slot, which the next
  unrelated `~~` then raised (YAMLish's failed `load-yaml` made `5 ~~ Int`
  throw "Type check failed in binding to parameter '$indent'"). A parse that
  returns instead of raising now clears the slot.
