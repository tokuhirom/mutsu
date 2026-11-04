use Test;

# From the Protocol distribution: `Routine.yada` on the Method objects
# `.^methods` and `.^lookup` hand back.
plan 5;

class Stubbed {
    method stub { ... }
    method real { 1 }
    method empty { }
}

my %yada = Stubbed.^methods(:local).map({ .name => .yada });
ok %yada<stub>, 'a method with a `...` body is a yada';
nok %yada<real>, 'a method with a body is not';
nok %yada<empty>, 'an empty method is not';
ok Stubbed.^lookup('stub').yada, 'also through .^lookup';
nok Stubbed.^lookup('real').yada, 'and a real one through .^lookup';
