# `my $f = my $g = ...` and `&!attr := sub`

Two gaps found with Test::Describe, whose three test files now print the same
ok/not-ok lines as rakudo.

- `my &ROOT = my $DESCRIBE = ...` compiled the inner declaration in expression
  position, which gave it no local slot: a closure or named sub declared later
  wrote a variable nobody else saw (`describe` swapped `$DESCRIBE`, `it` still
  saw the root). The inner declaration of a declaration's initializer is now
  hoisted and compiled as the statement it is.
- Calling a `&`-attribute that was bound with `:=` (`&!v := sub () { ... }`,
  then `&!v()`) handed the shared cell to the call, which died with "No such
  method 'CALL-ME' for invocant of type 'Sub'". Invoking a container now invokes
  what it holds.
