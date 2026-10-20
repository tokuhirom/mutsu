use v6;
use Test;

plan 3;

# A statement call to a routine sinks what the routine hands back (#11537):
# a routine that is not `is rw`/`is raw` decontainerizes its implicit result,
# so the value's user `sink` runs; a routine that hands back a container
# (`is rw`, `is raw`, `return-rw`, `return $x`) or a readonly parameter does
# not run it. Every expectation is the `raku` output.

my @log;
class S { has $.t; method sink { @log.push: $!t } }
my $x = S.new(:t<x>);
my @a = S.new(:t<a>);

sub g1 { $x }; g1;
sub g2 { @a[0] }; g2;
sub g3(\v) { v }; g3(S.new(:t<v>));
sub g4 { S.new(:t<fresh>) }; g4;
sub g5 { if True { $x } }; g5;
sub g6 { my $y = S.new(:t<local>); $y }; g6;
sub g7($v is copy) { $v }; g7(S.new(:t<copy>));
g1();

is @log.join(' '), 'x a v fresh x local copy x',
    'a decontainerized routine result runs its sink';

@log = ();
sub n1 is rw { $x }; n1;
sub n2 is raw { $x }; n2;
sub n3 { return-rw $x }; n3;
sub n4 { return $x }; n4;
sub n5($v) { $v }; n5($x);
sub n6(:$v) { $v }; n6(:v($x));
sub n7($v) { if True { $v } }; n7($x);
sub n8 { my $l = S.new(:t<l>); return $l }; n8;
my $kept = g1;

is @log.join(' '), '', 'a container or a readonly parameter is not sunk through';

@log = ();
sub mixed { [1] does role { method sink { @log.push: 'mixin' } } }; mixed;
is @log.join(' '), 'mixin', 'a mixin with a composed sink is sunk';
