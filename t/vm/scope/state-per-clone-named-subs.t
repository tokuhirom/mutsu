use v6;
use Test;

plan 6;

# Raku scopes a `state` variable to the routine CLONE: a named sub nested in
# another routine is cloned per enclosing call (its state re-initializes),
# while a top-level sub has one clone for the program (its state persists).
# Cro::ConnectionConditional.new relies on this (`state $first = True` in a
# nested `sub check-compatibility`).
sub outer() {
    sub inner() { state $n = 0; ++$n }
    inner(); inner()
}
is outer(), 2, 'nested named sub state counts within one enclosing call';
is outer(), 2, 'nested named sub state re-initializes per enclosing call';

sub counter { state $c = 0; ++$c }
is counter(), 1, 'top-level sub state initializes once';
is counter(), 2, 'top-level sub state persists across calls (2)';
is counter(), 3, 'top-level sub state persists across calls (3)';

# Two sibling invocations of the enclosing routine each get a fresh clone,
# and within one invocation repeated calls share the clone's state.
sub host($calls) {
    sub tick() { state $t = 0; ++$t }
    tick() for ^$calls;
    tick()
}
is host(3), 4, 'clone state accumulates within a single enclosing call';
