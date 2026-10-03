use Test;

# A WhateverCode position into a not-yet-vivified container is computed
# against the empty Array the write creates.

plan 3;

my %j;
sub g(\c) is rw { return-rw c[*-0] }
g(%j<a>) = 'five';
is-deeply %j, {a => ['five']}, 'return-rw c[*-0] on a missing hash entry';
g(%j<a>) = 'six';
is-deeply %j, {a => ['five', 'six']}, 'a second call appends';

my %m;
sub w(\c) { c[*-0] = 'y' }
w(%m<a>);
is-deeply %m, {a => ['y']}, 'direct assignment through c[*-0]';
