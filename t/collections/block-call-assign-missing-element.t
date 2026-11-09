use v6;
use Test;

# A block whose last statement is a subscript of a MISSING element hands back
# the location the write creates when the call is an assignment target, as an
# `is rw` routine does (Test::Mock's `overriding => { a => -> $i { state %a;
# %a{$i} } }`); a plain read of it still does not autovivify.
plan 8;

my %h;
my &f = -> $i { %h{$i} };
f("t") = 1;
is-deeply %h, {t => 1}, 'a pointy block assigns a missing hash key';

my @a;
my &g = -> $i { @a[$i] };
g(2) = 5;
is-deeply @a, [Any, Any, 5], 'a pointy block assigns a missing array index';

my &s = -> $i { state %st; %st{$i} };
s("k") = 13;
is s("k"), 13, 'a state hash behind the block keeps the assigned value';

my &b = { state %bt; %bt{$^i} };
b("z") = 4;
is b("z"), 4, 'a bare block with a placeholder does too';

my %r;
my &rd = -> $i { %r{$i} };
my $v = rd("none");
is-deeply %r, {}, 'reading a missing key through the block does not autovivify it';
ok !$v.defined, 'and reads as undefined';

my %e = t => 0;
my &ex = -> $i { %e{$i} };
ex("t") = 9;
is-deeply %e, {t => 9}, 'an existing key still assigns through the block';

sub rwsub($i) is rw { %e{$i} }
rwsub("n") = 3;
is-deeply %e, {t => 9, n => 3}, 'the is rw routine form agrees';
