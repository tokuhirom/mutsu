use Test;

# A method's `multi`, `!private`, `is rw` and `is raw` across the RakuAST
# boundary (ADR-10723 Stage 1). Measured on rakudo 2026.09:
# `multiness => "multi"` and `private => True` precede `name`, and a trait is
# `traits => (Trait::Is(name => …),)` before `body`. This file passes under
# both mutsu and raku. Distinct class names because `.AST` registers the
# symbol.

plan 13;

sub meth(Str $src) {
    $src.AST.statements[0].expression.body.body.statement-list.statements[0].expression
}

{
    my $m = meth(Q[class M1 { multi method a($x) { 1 } }]);
    is $m.multiness, 'multi', 'a multi method has multiness';
    like $m.gist, /'multiness => "multi",' \s* 'name'/, 'ahead of its name';
}

{
    my $m = meth(Q[class M2 { method !p() { 2 } }]);
    ok $m.private, 'a private method is private';
    like $m.gist, /'private => True,' \s* 'name'/, 'ahead of its name';
}

{
    my $m = meth(Q[class M3 { method r() is rw { 3 } }]);
    like $m.traits[0].gist, /'from-identifier("rw")'/, 'is rw is a Trait::Is';
    like $m.gist, /'traits => (' .* 'body'/, 'before the body';
}

like meth(Q[class M4 { method w() is raw { 4 } }]).traits[0].gist,
    /'from-identifier("raw")'/, 'is raw is a Trait::Is';

# Write direction.
is EVAL(Q[class N1 { multi method a(Int $x) { "int" }; multi method a(Str $x) { "str" } };
    N1.a(1) ~ N1.a("x")].AST), 'intstr', 'multi methods round-trip and dispatch';
is EVAL(Q[class N2 { method !p() { 7 }; method q() { self!p } }; N2.q].AST), 7,
    'a private method round-trips';
dies-ok { EVAL Q[class N3 { method !p() { 7 } }; N3.p].AST },
    'and is still not public';
{
    my $o = EVAL Q[class N4 { has $!v = 1; method v() is rw { $!v } }; N4.new].AST;
    $o.v = 5;
    is $o.v, 5, 'is rw round-trips and returns the container';
}
is EVAL(Q[class N5 { method w() is raw { 9 } }; N5.w].AST), 9, 'is raw round-trips';
is EVAL(Q[class N6 { multi method !m(Int) { "i" }; method go() { self!m(3) } }; N6.go].AST), 'i',
    'a private multi method round-trips';
