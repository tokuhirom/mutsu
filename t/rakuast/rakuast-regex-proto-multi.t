use Test;

# `proto token` / `proto regex` / `proto rule` and `multi token`, as rakudo 2026.09 renders
# them: a `TokenDeclaration` / `RegexDeclaration` / `RuleDeclaration` whose `multiness` is
# `proto` over a body that is only `{*}` (`OnlyStar`), or `multi` over the candidate's regex;
# `my proto token` adds `scope => "my"`. The round trip dispatches as the parsed program does.
#
# The tree part of this file also passes under `raku`; the round trip part is mutsu's.

plan 24;

sub decl($src) { $src.AST.statements.head.expression }
sub same($src, $expected, $desc) {
    my $parsed = do { my @*LOG; EVAL($src) };
    my $round = do { my @*LOG; EVAL($src.AST) };
    is "$parsed|$round", "$expected|$expected", $desc;
}

# --- the tree
{
    my $t = decl(Q[proto token foo {*}]);
    isa-ok $t, RakuAST::TokenDeclaration, '`proto token` is a token declaration';
    is $t.multiness, 'proto', 'whose multiness is `proto`';
    is $t.name.canonicalize, 'foo', 'with its name';
    isa-ok $t.body, RakuAST::OnlyStar, 'over the dispatcher';
    is $t.scope, 'has', 'with the default scope';
    isa-ok decl(Q[proto regex bar { * }]), RakuAST::RegexDeclaration, '`proto regex`';
    isa-ok decl(Q[proto rule baz {*}]), RakuAST::RuleDeclaration, '`proto rule`';
    is decl(Q[proto rule baz {*}]).multiness, 'proto', 'is a proto too';
    is decl(Q[my proto token pt {*}]).scope, 'my', '`my proto token` has the `my` scope';
    is decl(Q[our proto token pt {*}]).scope, 'our', 'and `our` its';
    is decl(Q[token plain { a }]).multiness, '', 'a plain declaration has an empty multiness';
}
{
    my $g = Q[grammar G { proto token t {*}; token t:sym<a> { a }; token t:sym<b> { b } }].AST.statements.head.expression;
    my $s = $g.body.body.statement-list.statements;
    is $s.elems, 3, 'a grammar with a proto and two candidates';
    isa-ok $s[0].expression.body, RakuAST::OnlyStar, 'the first is the dispatcher';
    is $s[1].expression.multiness, '', 'a `:sym<>` candidate is not marked';
}

# --- the round trip
same Q[grammar G1 { proto token t {*}; token t:sym<a> { a }; token t:sym<b> { b }; token TOP { <t>+ } }; ~G1.parse("abba")], 'abba',
    'a proto token dispatches to its candidates';
same Q[grammar G2 { proto token t {*}; token t:sym<x> { x }; token t:sym<y> { y }; token TOP { <t> } }; G2.parse("y")<t>.Str], 'y',
    'the matching candidate is the capture';
same Q[grammar G3 { proto regex r {*}; regex r:sym<one> { 1 }; regex r:sym<two> { 2 }; regex TOP { <r>+ } }; ~G3.parse("12")], '12',
    'a proto regex';
same Q[grammar G4 { proto rule r {*}; rule r:sym<w> { w }; rule TOP { <r> <r> } }; ~G4.parse("w w")], 'w w',
    'a proto rule';
same Q[my proto token mp {*}; my token mp:sym<k> { k }; ~("k" ~~ /<mp>/)], 'k',
    'a lexical proto token';
same Q[my grammar G5 { multi token mt { a }; token TOP { <mt> } }; ~G5.parse("a")], 'a',
    'a `multi token`';
same Q[grammar G6 { proto token t {*}; token t:sym<a> { a { $*n++ } }; token TOP { <t>+ } }; my $*n = 0; G6.parse("aaa"); $*n], 3,
    'a candidate\'s code block runs once per match';
same Q[grammar G7 { proto token t {*}; token t:sym<num> { \d+ }; token t:sym<word> { \w+ }; token TOP { <t> } }; G7.parse("12")<t>.Str], '12',
    'the longest-token candidate wins';
same Q[grammar G8 { token TOP { <t> }; proto token t($x?) {*}; token t:sym<z> { z } }; ~G8.parse("z")], 'z',
    'a proto declared after its user';
same Q[grammar G9 { proto token t {*}; token t:sym<a> { a }; token TOP { <t> } }; G9.parse("a").made.defined.so], 'False',
    'a grammar without actions makes nothing';
