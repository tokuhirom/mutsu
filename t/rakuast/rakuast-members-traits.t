use Test;

# The traits of attributes, methods and operator subs in RakuAST, measured on
# rakudo 2026.09:
#
# - an attribute's `traits` are in written order: `is rw`, `is required` (with
#   an optional reason), `is DEPRECATED`, a custom `is NAME(ARGS)`, `is TYPE`
#   (a container type: `Trait::Is(type => ...)`), `does ROLE`
#   (`Trait::Does`) and `handles TERM`;
# - `where` is the last field of an attribute; `our $.x` has the `our` scope,
#   `my $.x` none, and `has $x` (no twigil) no twigil;
# - a method has the `my` / `our` scope, and `is default`, `is DEPRECATED` and
#   custom traits as `Trait::Is`;
# - `sub infix:<+++> ... is assoc<left>` is `Trait::Is(name => assoc, argument =>
#   QuotedString)`, `is tighter(&infix:<+>)` a parenthesised `Var::Lexical`.
#
# The round trip is the parsed program. The tree part of this file also passes
# under `raku`; the round trip part is mutsu's.

plan 45;

sub exprs($src) { $src.AST.statements.map(*.expression) }
sub members($src) { exprs($src)[*-1].body.body.statement-list.statements.map(*.expression) }
sub traits-of($src) { members($src)[0].traits.map({ .name.defined ?? .name.canonicalize !! .^name }) }
sub same($src, $expected, $desc) {
    my $parsed = EVAL($src);
    my $round = EVAL($src.AST);
    is "$parsed|$round", "$expected|$expected", $desc;
}

# --- attribute traits keep their written order
{
    is-deeply traits-of(Q[class AT1 { has $.x is rw is required }]), ('rw', 'required').Seq,
        '`is rw is required` is in written order';
    is-deeply traits-of(Q[class AT2 { has $.x is required is rw }]), ('required', 'rw').Seq,
        'and the other way round';
    is-deeply traits-of(Q[class AT3 { has $.x is required is rw is built }]), ('required', 'rw', 'built').Seq,
        'three traits';
    my $d = members(Q[class AT4 { has $.x is built(False) is rw = 5 }])[0];
    is $d.traits.elems, 3, 'an initializer keeps the implicit WillBuild after the written traits';
    isa-ok $d.traits[2], RakuAST::Trait::WillBuild, 'it is last';
    isa-ok $d.initializer, RakuAST::Initializer::Assign, 'and the initializer stays';
}
{
    my $t = members(Q[class AT5 { has $.x is required("because") }])[0].traits[0];
    is $t.name.canonicalize, 'required', '`is required("why")` is named required';
    isa-ok $t.argument, RakuAST::Circumfix::Parentheses, 'with its reason as the argument';
    my $dep = members(Q[class AT6 { has $.x is DEPRECATED("y") }])[0].traits[0];
    is $dep.name.canonicalize, 'DEPRECATED', '`is DEPRECATED("y")`';
    isa-ok $dep.argument, RakuAST::Circumfix::Parentheses, 'has an argument';
    nok members(Q[class AT7 { has $.x is DEPRECATED }])[0].traits[0].argument.defined, 'and the bare form has none';
}
{
    my $i = members(Q[class AT8 { has @.x is Buf; has %.h is BagHash }]);
    isa-ok $i[0].traits[0], RakuAST::Trait::Is, '`is Buf` is a Trait::Is';
    isa-ok $i[0].traits[0].type, RakuAST::Type::Simple, 'over a type';
    isa-ok $i[1].traits[0].type, RakuAST::Type::Simple, 'on a hash too';
    my @c = exprs(Q[multi sub trait_mod:<is>(Attribute $a, :$marked!) { }; class AT9 { has $.x is marked(5) is rw; has $.y is marked(1, 2); has $.z is marked }]);
    my @m = @c[1].body.body.statement-list.statements.map(*.expression);
    is @m[0].traits[0].name.canonicalize, 'marked', 'a custom trait is a Trait::Is by name';
    isa-ok @m[0].traits[0].argument, RakuAST::Circumfix::Parentheses, 'with its argument';
    like @m[1].traits[0].argument.gist, /'ApplyListInfix'/, 'a list of arguments is a comma list';
    nok @m[2].traits[0].argument.defined, 'a bare custom trait has no argument';
    is @m[0].traits[1].name.canonicalize, 'rw', 'and the next trait follows it';
    my @r = exprs(Q[role AR1 { }; class AT10 { has $.x does AR1 }]);
    my $does = @r[1].body.body.statement-list.statements[0].expression.traits[0];
    isa-ok $does, RakuAST::Trait::Does, '`does ROLE` is a Trait::Does';
}

# --- where, scope and alias
{
    my @w = members(Q[class AW1 { has Int $.x where * > 0; has Int $.y where { $_ > 1 } = 3 }]);
    isa-ok @w[0].where, RakuAST::ApplyInfix, '`where` is a field of the attribute';
    isa-ok @w[1].where, RakuAST::Block, 'a block constraint too';
    isa-ok @w[1].initializer, RakuAST::Initializer::Assign, 'beside an initializer';
    my @s = members(Q[class AS1 { our $.o = 5; my $.m = 6; our Int @.a }]);
    is @s[0].scope, 'our', '`our $.x` has the `our` scope';
    nok @s[1].scope.defined, '`my $.x` has none';
    is @s[2].scope, 'our', 'a typed array too';
    my $alias = members(Q[class AA1 { has Int $x = 3 }])[0];
    is $alias.scope, 'has', '`has $x` has the `has` scope';
    nok $alias.twigil.defined, 'and no twigil';
}

# --- methods
{
    my @m = members(Q[class MM1 { my method a { 1 }; our method b { 2 }; my multi method c(Int $x) { 3 }; my method !d { 4 } }]);
    like @m[0].gist, /'scope => "my"'/, '`my method` has the `my` scope';
    like @m[1].gist, /'scope => "our"'/, '`our method` the `our` scope';
    is @m[2].multiness, 'multi', '`my multi method` is multi';
    like @m[2].gist, /'scope     => "my"'/, 'and `my`';
    ok @m[3].private, 'a private lexical method';
    my @t = members(Q[class MM2 { multi method a is default { 1 }; method b is DEPRECATED("n") { 2 }; method c is DEPRECATED { 3 } }]);
    is @t[0].traits[0].name.canonicalize, 'default', '`is default` is a Trait::Is';
    is @t[1].traits[0].name.canonicalize, 'DEPRECATED', '`is DEPRECATED("n")`';
    nok @t[2].traits[0].argument.defined, 'and the bare form';
    my @c = exprs(Q[multi sub trait_mod:<is>(Method $m, :$tagged!) { }; class MM3 { method a is tagged(3) { 1 } }]);
    is @c[1].body.body.statement-list.statements[0].expression.traits[0].name.canonicalize, 'tagged',
        'a custom method trait';
}

# --- operator subs
{
    my $a = exprs(Q[sub infix:<+++>($a, $b) is assoc<left> { 1 }])[0].traits[0];
    is $a.name.canonicalize, 'assoc', '`is assoc<left>`';
    isa-ok $a.argument, RakuAST::QuotedString, 'has a quoted argument';
    my $p = exprs(Q[sub infix:<++++>($a, $b) is tighter(&infix:<+>) { 1 }])[0].traits[0];
    is $p.name.canonicalize, 'tighter', '`is tighter(&infix:<+>)`';
    isa-ok $p.argument, RakuAST::Circumfix::Parentheses, 'has a parenthesised argument';
}

# --- the round trip is the parsed program
same Q[class RT1 { has $.x is rw is required }; my $o = RT1.new(x => 1); $o.x = 5; $o.x], 5, 'rw and required together';
same Q[class RT1b { has $.x is rw is required }; (try { RT1b.new; "lived" }) // "died"], 'died', 'a required attribute is required';
same Q[class RT9 { has $.x is required("because") }; try RT9.new; ($!.message ~~ /because/).so], 'True', 'with its reason';
same Q[class RT2 { has %.h is BagHash }; RT2.new.h.^name], 'BagHash', 'a container type trait';
same Q[my @log; multi sub trait_mod:<is>(Attribute $a, :$marked!) { @log.push("m:" ~ $marked.raku) }; class RT3 { has $.x is marked(5) is rw }; @log.join(",")], 'm:5', 'a custom attribute trait';
same Q[my @log; multi sub trait_mod:<is>(Attribute $a, :$marked!) { @log.push("m:" ~ $marked.raku) }; multi sub trait_mod:<is>(Attribute $a, :$other!) { @log.push("o:" ~ $other.raku) }; class RT3c { has $.x is other(1) is marked is other(2) }; @log.join(",")], 'o:1,m:Bool::True,o:2', 'custom traits run in written order';
same Q[class RT5 { has Int $.x where * > 0 }; (try { RT5.new(x => -1); "ok" }) // "fail"], 'fail', 'an attribute `where` constraint';
same Q[class RT5b { has Int $.x where * > 0 }; RT5b.new(x => 3).x], 3, 'that accepts a good value';
same Q[class RT7 { has $x = 4; method m { $x } }; RT7.new.m], 4, 'the alias of a private attribute';
same Q[class RT6 { our $.x = 5 }; RT6.x], 5, 'an `our` attribute';
same Q[class RT6b { my $.x = 5 }; RT6b.x], 5, 'a `my` attribute';
same Q[class RM1 { my method m { "m" }; method call { self.&m } }; RM1.new.call], 'm', 'a lexical method';
same Q[class RM3 { multi method m(Int $x) is default { "a" }; multi method m(Int $x) { "b" } }; RM3.new.m(1)], 'a', '`is default` breaks a tie';
same Q[my @log; multi sub trait_mod:<is>(Method $m, :$tagged!) { @log.push("t:" ~ $tagged.raku) }; class RM5b { method m is tagged(3) { 1 } }; @log.join(",")], 't:3', 'a custom method trait';
same Q[sub infix:<+++>($a, $b) is assoc<left> { "($a $b)" }; 1 +++ 2 +++ 3], '((1 2) 3)', 'left associativity';
same Q[sub infix:<***>($a, $b) is assoc<right> { "($a $b)" }; 1 *** 2 *** 3], '(1 (2 3))', 'right associativity';
same Q[sub infix:<⊕>($a, $b) is tighter(&infix:<*>) { $a + $b * 10 }; 2 * 3 ⊕ 4], 86, 'a tighter operator';
same Q[sub infix:<⊖>($a, $b) is looser(&infix:<+>) { "[$a $b]" }; 1 + 2 ⊖ 3 + 4], '[3 7]', 'a looser operator';
same Q[sub infix:<⊗>($a, $b) is equiv(&infix:<+>) { "[$a $b]" }; 1 + 2 ⊗ 3], '[3 3]', 'an equivalent operator';
same Q[sub dep() is DEPRECATED("x") { 7 }; dep()], 7, 'a deprecated sub';
