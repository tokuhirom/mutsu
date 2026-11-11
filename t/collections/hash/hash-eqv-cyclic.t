use Test;

# `eqv` over a container that holds itself (#16888, EEL 0.3's cyclic partition
# key) compares the cycle once instead of recursing until the stack overflows.

plan 4;

my %cycle; %cycle<self> := %cycle;
ok %cycle eqv %cycle, 'a self-holding hash is eqv itself';
my @loop; @loop[0] := @loop;
ok @loop eqv @loop, 'a self-holding array is eqv itself';
my %plain = self => 1;
nok %cycle eqv %plain, 'a cyclic hash is not eqv a plain one';
ok %cycle === %cycle, 'nor does identity recurse';
