use Test;
plan 8;

# ADR-0021 P2 (I4): named-ness of a slipped argument depends on what kind
# of container is being slipped, not on the flavour an individual Pair
# element happens to carry. `|@l` / `|$list` always produce POSITIONAL
# arguments (containerizing any Pair elements); `|$pair` and `|%h` always
# produce NAMED arguments.

multi g(Pair $p) { 'positional:' ~ $p.key ~ '=' ~ $p.value }
multi g(:$x!) { 'named:x=' ~ $x }

my @l = (x => 1,);
is g(|@l), 'positional:x=1', 'slipping an array of pairs is positional';

my $p = x => 1;
is g(|$p), 'named:x=1', 'slipping a bare Pair variable is named';

sub take_named(:$x!, :$y!) { "named x=$x y=$y" }
my %h = x => 1, y => 2;
is take_named(|%h), 'named x=1 y=2', 'slipping a Hash is named';

# Forwarding a positional Pair through `|c` must stay positional.
sub inner(Pair $p) { 'inner positional:' ~ $p.key ~ '=' ~ $p.value }
sub inner_via_capture($outer_p) {
    sub relay(|c) { inner(|c) }
    relay($outer_p);
}
is inner_via_capture($p), 'inner positional:x=1',
    'outer($p) -> inner(|c) forwarding stays positional';

# callsame with a Pair positional must dispatch to the Pair candidate.
class B {
    multi method m(Pair $p) { 'B positional:' ~ $p.key ~ '=' ~ $p.value }
    multi method m(:$x!) { 'B named:x=' ~ $x }
}
class D is B {
    multi method m(Pair $p) { 'D->' ~ callsame }
    multi method m(:$x!) { 'D->' ~ callsame }
}
is D.new.m($p), 'D->B positional:x=1', 'callsame with a Pair positional reaches the Pair candidate';

# The `.subst(|(:g), ...)` / `.subst(|($k => v), ...)` adverb-promotion
# shape stays named -- a *bare* Pair slip, not an array-of-pairs slip.
my $str = "1234567";
$str.subst(|(nth => 1..3), /../, 'XX');
is +$/, 3, 'subst adverb promotion via bare pair slip still finds 3 matches';

# Array of pairs used as a positional slurpy still binds elements as Pairs.
sub slurpy(*@rest) { @rest.map({.WHAT.gist}).join(',') }
is slurpy(|@l), '(Pair)', 'slurpy positional binding still sees Pair-typed elements';

# A bareword fat-arrow written inside a generic list literal (not directly
# as a call argument) is NOT a call-site named arg -- slipping that list
# containerizes every element, including ones with a literal bareword key.
multi h(Pair $p1, Pair $p2) { 'both positional:' ~ $p1.key ~ '=' ~ $p1.value ~ ',' ~ $p2.key ~ '=' ~ $p2.value }
is h(|(@l[0], x => 9)), 'both positional:x=1,x=9',
    'a literal bareword pair inside a slipped list literal is positional, not named';
