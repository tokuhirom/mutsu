use Test;

# The header of a package-like declaration in RakuAST, measured on rakudo
# 2026.09: the scope (`my`, `unit`), the adverbs of the name and `is export`.
#
# - `class A:ver<1.0>:auth<me> { }` is a `Class` whose `name` is a Name with
#   `colonpairs` (`ColonPair::Value(key => "ver", value => <words val>)`);
# - `is export` / `is export(:tag)` is a `Trait::Is` in `traits`, for a class,
#   a grammar, a module, an enum and a subset (before a subset's `of`);
# - `my` / `unit` is a leading `scope`; the body of `unit module M;` is the rest
#   of the unit, which mutsu's parser leaves beside the declaration.
#
# mutsu's parser spells the adverbs as meta setters and the export as a
# registration around the declaration; the lowering rebuilds those, so the
# round trip is the parsed program. The tree part also passes under `raku`; the
# round trip part declares each name twice in one process, which raku refuses.

plan 64;

sub exprs($src) { $src.AST.statements.map(*.expression) }
sub run($src) { my $parsed = EVAL($src); my $round = EVAL($src.AST); ($parsed, $round) }
sub same($src, $expected, $desc) {
    my ($parsed, $round) = run($src);
    is "$parsed|$round", "$expected|$expected", $desc;
}

# --- the tree
{
    my @c = exprs(Q[class A1 is export { }; class A2 is export(:foo) { }; class A3 is export(:foo, :bar) { }]);
    isa-ok @c[0], RakuAST::Class, '`class A is export` is a Class';
    is @c[0].traits.elems, 1, 'with one trait';
    isa-ok @c[0].traits[0], RakuAST::Trait::Is, 'a Trait::Is';
    is @c[0].traits[0].name.canonicalize, 'export', 'named export';
    nok @c[0].traits[0].argument.defined, 'bare, with no argument';
    isa-ok @c[1].traits[0].argument, RakuAST::Circumfix::Parentheses, '`export(:foo)` has an argument';
    like @c[2].traits[0].argument.gist, /'ColonPair::True.new("foo")' .* 'ColonPair::True.new("bar")'/,
        'a list of tags is a list of colonpairs';
}
{
    my @e = exprs(Q[class B1:ver<1.0>:auth<me> { }]);
    like @e[0].gist, /'Name.from-identifier("B1", colonpairs => ('/, 'the adverbs are colonpairs of the name';
    like @e[0].gist, /'key   => "ver"' .* 'key   => "auth"'/, 'in the written order';
    like @e[0].gist, /'processors => <words val>'/, 'with word-quoted values';
    my @m = exprs(Q[module M1:ver<v1.2> is export { }]);
    isa-ok @m[0], RakuAST::Module, 'a module with an adverb and `is export`';
    like @m[0].gist, /'colonpairs' .* 'Trait::Is'/, 'has both';
}
{
    my @g = exprs(Q[grammar G1 is export { }; my class L1 is export { }]);
    isa-ok @g[0], RakuAST::Grammar, '`grammar G is export` is a Grammar';
    is @g[0].traits.elems, 1, 'with the export trait';
    isa-ok @g[1], RakuAST::Class, 'a lexical `my class is export` is a Class';
    is @g[1].scope, 'my', 'with the `my` scope';
    is @g[1].traits.elems, 1, 'and the export trait';
}
{
    my @s = exprs(Q[my module M2 { }; my package P2 { }; my enum E2 <a b>; my subset S2 where * > 0]);
    is @s[0].scope, 'my', '`my module` has the `my` scope';
    is @s[1].scope, 'my', '`my package` has it';
    isa-ok @s[2], RakuAST::Type::Enum, '`my enum` is a Type::Enum';
    is @s[2].scope, 'my', 'with the `my` scope';
    isa-ok @s[3], RakuAST::Type::Subset, '`my subset` is a Type::Subset';
    is @s[3].scope, 'my', 'with the `my` scope';
}
{
    my @e = exprs(Q[enum E3 is export <a b>; enum E4 is export(:foo) <c d>]);
    is @e[0].traits[0].name.canonicalize, 'export', '`enum E is export` has the export trait';
    isa-ok @e[1].traits[0].argument, RakuAST::Circumfix::Parentheses, 'with its tags';
    my @s = exprs(Q[subset S3 is export of Int where * > 0]);
    is @s[0].traits.elems, 2, '`subset S is export of Int` has two traits';
    isa-ok @s[0].traits[0], RakuAST::Trait::Is, 'the export first';
    isa-ok @s[0].traits[1], RakuAST::Trait::Of, 'then the base type';
}
{
    my $u = Q[unit module M3; sub f { 1 }; f()].AST;
    is $u.statements.elems, 1, '`unit module M;` holds the rest of the unit';
    my $m = $u.statements[0].expression;
    isa-ok $m, RakuAST::Module, 'it is a Module';
    is $m.scope, 'unit', 'with the `unit` scope';
    is $m.body.body.statement-list.statements.elems, 2, 'and the rest in its body';
    my $c = exprs(Q[unit class C3; method m { 1 }]);
    is $c[0].scope, 'unit', '`unit class` has the `unit` scope';
}

{
    my @h = exprs(Q[class H1 is hidden { }; class H2 { }; class H3 hides H2 { }; class H4 is H2 is hidden { }]);
    is @h[0].traits[0].name.canonicalize, 'hidden', '`is hidden` is a Trait::Is named hidden';
    isa-ok @h[2].traits[0], RakuAST::Trait::Hides, '`hides H2` is a Trait::Hides';
    is @h[2].traits.elems, 1, 'and the hidden parent is not also an `is` trait';
    isa-ok @h[3].traits[0], RakuAST::Trait::Is, '`is H2 is hidden` keeps the parent first';
    is @h[3].traits[1].name.canonicalize, 'hidden', 'and then `hidden`';
    my @t = exprs(Q[class T1 { }; class T2 { trusts T1; }]);
    my $trusts = @t[1].body.body.statement-list.statements[0];
    isa-ok $trusts, RakuAST::Statement::Trusts, '`trusts T1` is a Statement::Trusts';
    isa-ok $trusts.type, RakuAST::Type::Simple, 'with the type';
    my @a = Q[use MONKEY-TYPING; augment class Int { method foo { 1 } }].AST.statements;
    isa-ok @a[1].expression, RakuAST::Class, '`augment class` is a Class';
    is @a[1].expression.scope, 'augment', 'with the `augment` scope';
    my @p = exprs(Q[our proto sub pf(|) {*}]);
    is @p[0].scope, 'our', '`our proto sub` has the `our` scope';
    is @p[0].multiness, 'proto', 'and the `proto` multiness';
}

# --- the round trip is the parsed program
same Q[module M4:ver<1.2> { }; M4.^ver.Str], '1.2', 'a module version survives';
same Q[class V1:ver<1.0>:auth<me> { }; V1.^ver.Str ~ "|" ~ V1.^auth], '1.0|me', 'class version and auth survive';
same Q[class V2:api<3> { }; V2.^api], '3', 'and the api';
same Q[module Ex1 { class Ex1::K is export { method m { 7 } } }; Ex1::K.new.m], 7, 'an exported class';
same Q[module Ex2 { enum Ex2::Color is export <Red Green>; }; Ex2::Color::Green.Str], 'Green', 'an exported enum';
same Q[module Ex3 { subset Ex3::Pos is export of Int where * > 0; }; (5 ~~ Ex3::Pos).Str], 'True', 'an exported subset';
same Q[module Ex4 { grammar Ex4::G is export { token TOP { a } } }; (Ex4::G.parse("a") ?? "y" !! "n")], 'y', 'an exported grammar';
same Q[{ my module M5 { our sub f { 5 } }; M5::f() }], 5, 'a lexical module is usable in its block';
same Q[my enum E5 <p q r>; E5::q.Int], 1, 'a lexical enum';
same Q[my subset S5 of Int where * > 2; (3 ~~ S5).Str], 'True', 'a lexical subset';
same Q[unit module M6; sub f { 6 }; f()], 6, 'the rest of a `unit module` is its body';
is EVAL(Q[unit class C6; method m { 6 }].AST).new.m, 6, 'and of a `unit class`';
same Q[unit package P7; our $x = 7; $P7::x], 7, 'and of a `unit package`';
same Q[my class L8 is export { method m { 8 } }; L8.new.m], 8, 'a lexical exported class';
same Q[class W9 is rw is export { has $.a is rw }; my $w = W9.new(a => 1); $w.a = 9; $w.a], 9, 'rw and export together';
same Q[class Hd1 is hidden { method m { 1 } }; Hd1.new.m], 1, 'a hidden class';
same Q[class Hd2 { method who { "base" } }; class Hd3 hides Hd2 { method who { callsame } }; Hd3.new.who], 'base', 'a class that hides its parent';
same Q[class Tr2 { trusts Tr1; has $!x2 = 7; method x2 { $!x2 } }; class Tr1 { method peek(Tr2 $o) { $o!Tr2::x2 } }; Tr1.new.peek(Tr2.new)], 7, 'a trusts declaration';
same Q[use MONKEY-TYPING; augment class Int { method triple { self * 3 } }; 4.triple], 12, 'an augmented class';
same Q[our proto sub opf($x) {*}; our multi sub opf(Int $x) { $x + 1 }; opf(2)], 3, 'an `our proto`';
