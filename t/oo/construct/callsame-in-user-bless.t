use v6;
use Test;

# `Mu.bless` is the base candidate of a user `method bless`: `callsame`,
# `nextsame` and `callwith` inside it reach it (#16335).
plan 5;

class C1 { has $.x; method bless(*%_) { callsame } }
is C1.bless(x => 1).x, 1, 'callsame reaches Mu.bless';

class C2 { has $.x; method bless(*%_) { nextsame } }
is C2.bless(x => 5).x, 5, 'nextsame reaches Mu.bless';

my $calls = 0;
class C3 { has $.x; method bless(*%_) { $calls++; callwith(|%_) } }
is C3.bless(x => 2).x, 2, 'callwith forwards its arguments to Mu.bless';
is $calls, 1, 'the user method ran once';

class C4 { has $.x; has $.y; method bless(*%_) { callwith(|%_, y => 7) } }
my $o = C4.bless(x => 3);
is "{$o.x} {$o.y}", '3 7', 'callwith can change the arguments';
