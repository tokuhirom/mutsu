use v6;
use Test;

# From Test::Selector (via MIDI::Make): `class :: { ... }.new xx 2` evaluates its
# operand twice, which reached the compile-time class-redeclaration check twice
# for the one parser-minted anonymous class name and died with
# "Redeclaration of symbol '__ANON_CLASS_0__'".
plan 3;

sub twice() {
    my $c = '';
    with class :: { method m { 1 } }.new xx 2 -> ($x, $y) { $c ~= $x.m ~ $y.m }
    $c
}

is twice(), '11', 'an anonymous class in the operand of xx';
is twice(), '11', 'and again on a second call';

my @seen;
for ^2 {
    my $x = class { method m { 42 } }.new;
    @seen.push: $x.m;
}
is @seen, [42, 42], 'an anonymous class declared in a loop body still works';
