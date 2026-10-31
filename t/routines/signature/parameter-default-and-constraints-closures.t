use Test;

# `Parameter.default` and `.constraints` hand back code objects; repeated
# introspection of one signature answers working closures every time.

plan 8;

sub f($x where { $_ > 2 } = 5, $y where * > 10 = 20) { }
my @params = &f.signature.params;

is @params[0].default.(), 5, '.default runs the default expression';
is @params[1].default.(), 20, '.default of the second parameter';
for 1..2 {
    ok 3 ~~ @params[0].constraints, 'a block constraint accepts a passing value';
    nok 1 ~~ @params[0].constraints, 'and rejects a failing one';
}
ok 11 ~~ @params[1].constraints, 'a WhateverCode constraint accepts';
nok 3 ~~ @params[1].constraints, 'and rejects';
