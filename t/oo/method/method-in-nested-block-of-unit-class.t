use lib 't/lib';
use Test;

# The `unit class` form gathers the rest of the file as the class body, so a
# method declared in a nested block there is the class's too (#9525).

plan 2;

use NestedBlockMethodUnit;

is-deeply NestedBlockMethodUnit.new.unrecord, (10, 20, 30),
    'a do-block method in a unit class satisfies its role and reads the block sub';
ok NestedBlockMethodUnit.^methods(:local).first(*.name eq 'unrecord'),
    'the method is in the method table';
