use Test;

# Conjunctions, interpolated expressions and aliases of a variable or a class in a regex, as
# rakudo 2026.09 renders them:
#
# - `a & b` is a `Regex::Conjunction` and `a && b` a `Regex::SequentialConjunction`, each
#   holding its operands; `&&` binds looser than `&`, and both tighter than `|`;
# - `$(EXPR)`, `@(EXPR)` is a `Regex::Interpolation` whose `var` is a `Contextualizer::Item` /
#   `::List` over a `StatementSequence`;
# - `<rx=$r>` is an `Assertion::Alias` over an `Assertion::InterpolatedVar`, `<foo=[bao]>`
#   one over an `Assertion::CharClass`.
#
# The round trip is the parsed program. The tree part of this file also passes under `raku`;
# the round trip part is mutsu's.

plan 30;

sub body($src) { $src.AST.statements.head.expression.body }
sub unws($n) { $n ~~ RakuAST::Regex::WithWhitespace ?? $n.regex !! $n }
sub same($src, $expected, $desc) {
    my $parsed = do { my @*LOG; EVAL($src) };
    my $round = do { my @*LOG; EVAL($src.AST) };
    is "$parsed|$round", "$expected|$expected", $desc;
}

# --- conjunctions
{
    my $c = body(Q[/a & b/]);
    isa-ok $c, RakuAST::Regex::Conjunction, '`a & b` is a conjunction';
    is $c.branches.elems, 2, 'of two operands';
    isa-ok unws($c.branches[0]), RakuAST::Regex::Literal, 'the first `a`';
    isa-ok body(Q[/a && b/]), RakuAST::Regex::SequentialConjunction, '`a && b` is a sequential conjunction';
    is body(Q[/a & b & c/]).branches.elems, 3, 'three operands stay in one node';
    my $m = body(Q[/a & b && c/]);
    isa-ok $m, RakuAST::Regex::SequentialConjunction, '`&&` binds looser than `&`';
    isa-ok $m.branches[0], RakuAST::Regex::Conjunction, 'so `a & b && c` holds `a & b` first';
    my $a = body(Q[/a | b & c/]);
    isa-ok $a, RakuAST::Regex::Alternation, 'a conjunction is tighter than `|`';
    isa-ok $a.branches[1], RakuAST::Regex::Conjunction, 'inside the second branch';
    isa-ok body(Q[/[a && b]/]).regex, RakuAST::Regex::SequentialConjunction, 'in a group';
}

# --- interpolated expressions
{
    my $r = ('my $r; ' ~ Q[/$($r)/]).AST.statements[1].expression.body;
    isa-ok $r, RakuAST::Regex::Interpolation, '`$($r)` is an interpolation';
    isa-ok $r.var, RakuAST::Contextualizer::Item, 'of an item contextualizer';
    isa-ok $r.var.target, RakuAST::StatementSequence, 'over a statement sequence';
    isa-ok ('my @a; ' ~ Q[/@(@a)/]).AST.statements[1].expression.body.var, RakuAST::Contextualizer::List,
        '`@(...)` is a list contextualizer';
    my $e = ('my $r; ' ~ Q[/$( $r + 1 )/]).AST.statements[1].expression.body.var.target.statements[0].expression;
    isa-ok $e, RakuAST::ApplyInfix, 'any expression may be inside';
}

# --- aliases of a variable or a class
{
    my $a = ('my $r; ' ~ Q[/<rx=$r>/]).AST.statements[1].expression.body;
    isa-ok $a, RakuAST::Regex::Assertion::Alias, '`<rx=$r>` is an alias';
    is $a.name, 'rx', 'named `rx`';
    isa-ok $a.assertion, RakuAST::Regex::Assertion::InterpolatedVar, 'over an interpolated variable';
    my $c = body(Q[/<foo=[bao]>/]);
    isa-ok $c, RakuAST::Regex::Assertion::Alias, '`<foo=[bao]>` is an alias';
    isa-ok $c.assertion, RakuAST::Regex::Assertion::CharClass, 'over a character class';
}

# --- the round trip is the parsed program
same Q[~("ab" ~~ /\w+ & <[a..c]>+/)], 'ab', 'a conjunction';
same Q[~("abc" ~~ /a.c & ab./)], 'abc', 'both operands match the same text';
same Q[~("abc" ~~ /abc && <alpha>+/)], 'abc', 'a sequential conjunction';
same Q[~("ab" ~~ /[ a & a ] b/)], 'ab', 'a conjunction in a group';
same Q[my $r = "b"; ~("abc" ~~ /a $($r) c/)], 'abc', 'an interpolated expression';
same Q[my $n = 1; ~("ab2" ~~ /ab $($n + 1)/)], 'ab2', 'a computed interpolation';
same Q[my @alts = <x y>; ~("y" ~~ /@(@alts)/)], 'y', 'an interpolated list';
same Q[my $r = rx/b/; ~("abc" ~~ /a <rx=$r> c/)], 'abc', 'an aliased variable';
same Q[~("xay" ~~ /x <foo=[bao]>+ y/)], 'xay', 'an aliased class';
same Q[~("ab" ~~ /<foo=[bao]>+/)<foo>.join(",")], 'a,b', 'the alias is the capture name';
