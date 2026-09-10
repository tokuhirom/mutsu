use Test;

# A non-suppressing alias `<x=rule>` files ONE capture under both `x` and
# `rule` (raku: `$<x> === $<rule>`), so its action method must fire exactly
# once. mutsu used to store two independent copies of the node and dispatch
# each of them, which fired the whole matched subtree's actions twice per
# alias level -- compounding to 2^depth for nested aliases (a leaf action ran
# 256 times on `benchmarks/bench-yaml-parse.raku`, whose YAMLish grammar
# aliases `<str=space>` and friends eight levels deep).

plan 8;

my %fired;

grammar Flat {
    token TOP { <x=leaf> }
    token leaf { 'x' }
}
grammar Nested {
    token TOP { <a=one> }
    token one { <b=two> }
    token two { <c=leaf> }
    token leaf { 'x' }
}
grammar Alternation {
    token TOP { [ <c=a> | <c=b> ]* }
    token a { 'x' }
    token b { 'y' }
}

class Actions {
    method leaf($/) { %fired<leaf>++; make 'LEAF' }
    method one($/)  { %fired<one>++ }
    method two($/)  { %fired<two>++ }
    method a($/)    { %fired<a>++ }
    method b($/)    { %fired<b>++ }
}

%fired = ();
my $flat = Flat.parse('x', :actions(Actions));
is %fired<leaf>, 1, 'aliased capture fires its action once';
is $flat<x>.made, 'LEAF', 'the alias name carries .made';
is $flat<leaf>.made, 'LEAF', 'the rule name carries the same .made';
ok $flat<x> === $flat<leaf>, 'both names are the same Match object';

%fired = ();
Nested.parse('x', :actions(Actions));
is %fired<leaf>, 1, 'nested aliases do not multiply the leaf action';
is-deeply (%fired<one>, %fired<two>), (1, 1), 'each intermediate alias fires once';

%fired = ();
Alternation.parse('xyx', :actions(Actions));
is %fired<a>, 2, 'aliased alternative fires once per match (a)';
is %fired<b>, 1, 'aliased alternative fires once per match (b)';
