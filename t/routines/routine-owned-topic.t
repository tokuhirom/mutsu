use Test;

# A routine owns its `$_`: it starts as `Any` in every call, on every call
# path, and a block reads the `$_` of the scope it is written in, never its
# caller's (ADR-12529 phase 3). The routine's frame root answers the fresh
# `$_` instead of a per-call insert. Every expectation was checked against
# rakudo.

plan 20;

$_ = 'caller topic';

sub named-no-params() { $_ }
sub light-positional($x) { $_ }
sub typed-light(Int $x) { $_ }
sub with-named(:$n) { $_ }
sub default-from-topic($x = $_) { $x }
class C {
    method plain() { $_ }
    method with-param($x) { $_ }
    method makes-a-block() { my $b = { $_ }; $b() }
}

given 'given topic' {
    is named-no-params().raku, 'Any', 'a sub without parameters starts with Any';
    is light-positional(1).raku, 'Any', 'a sub with a positional parameter starts with Any';
    is typed-light(1).raku, 'Any', 'a sub with a typed parameter starts with Any';
    is with-named(:n(1)).raku, 'Any', 'a sub with a named parameter starts with Any';
    is default-from-topic().raku, 'Any', 'a parameter default reads the routine\'s own $_';
    is C.plain.raku, 'Any', 'a method starts with Any';
    is C.with-param(1).raku, 'Any', 'a method with a parameter starts with Any';
    is C.makes-a-block.raku, 'Any', 'a block made in a method reads the method\'s $_';
    is (-> { $_ })().raku, '"given topic"', 'a pointy block reads the $_ of its scope';
}

sub block-in-sub() { my $b = { $_ }; $b() }
is block-in-sub().raku, 'Any', 'a block made in a sub reads the sub\'s $_';

sub bare-block-in-sub() { { $_ }() }
is bare-block-in-sub().raku, 'Any', 'a bare block called in a sub reads the sub\'s $_';

sub maker() { return { $_ } }
my $made = maker();
given 'later caller' {
    is $made().raku, 'Any', 'a block made in a sub that set no $_ does not read its caller\'s';
}

sub maker-set() { $_ = 'maker'; return { $_ } }
my $made-set = maker-set();
given 'later caller' {
    is $made-set().raku, '"maker"', 'a block reads the $_ its sub set';
}

sub run-block(&c) { $_ = 'runner'; c() }
is run-block({ $_ }), 'caller topic', 'a block called by another routine reads its own scope\'s $_';

sub writes-topic() { $_ = 5; $_ * 2 }
is writes-topic(), 10, 'a sub writes its own $_';
is $_, 'caller topic', '... which does not reach its caller';

sub map-in-sub() { (1, 2).map({ $_ * 10 }).join(',') }
is map-in-sub(), '10,20', 'a .map block binds its own $_';

sub when-in-sub($x) { given $x { when 1 { 'one' }; default { 'other' } } }
is when-in-sub(1) ~ when-in-sub(2), 'oneother', 'given/when in a sub';

sub return-from-block() { my &b = -> { return 'R' }; my $x = b(); "ran on with $x" }
is return-from-block(), 'R', 'a block\'s return leaves the sub that bound no name';

my $whatever = do { $_ = 42; (Int).map(*.new($_)).head };
is $whatever, 42, 'a WhateverCode reads the $_ of its scope';
