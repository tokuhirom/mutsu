use Test;

# Attribute traits across the RakuAST boundary (ADR-10723 Stage 1). Measured
# on rakudo 2026.09: `has $.x is rw` is a `VarDeclaration::Simple` whose
# `traits` list holds `Trait::Is(name => Name.from-identifier("rw"))`; an
# `= EXPR` default adds an implicit `Trait::WillBuild` that `.traits` answers
# but the gist omits. This file passes under both mutsu and raku. Distinct
# class names because `.AST` registers the symbol.

plan 15;

sub attr(Str $src) {
    $src.AST.statements[0].expression.body.body.statement-list.statements[0].expression
}

{
    my $a = attr(Q[class A1 { has $.x is rw }]);
    is $a.traits.elems, 1, 'is rw is one trait';
    isa-ok $a.traits[0], RakuAST::Trait::Is, 'a Trait::Is';
    like $a.traits[0].gist, /'from-identifier("rw")'/, 'naming rw';
    like $a.gist, /'desigilname => ' .* 'traits      => ('/, 'traits follow desigilname';
}

{
    my $a = attr(Q[class A2 { has Int $.y is required }]);
    like $a.traits[0].gist, /'from-identifier("required")'/, 'is required';
}

{
    my $a = attr(Q[class A3 { has $.z is rw = 5 }]);
    is $a.traits.map(*.^name).join(','), 'RakuAST::Trait::Is,RakuAST::Trait::WillBuild',
        'a default adds an implicit WillBuild after the written trait';
    nok $a.gist.contains('WillBuild'), 'which the gist omits';
    like $a.gist, /'initializer => RakuAST::Initializer::Assign.new('/, 'and the default is the initializer';
}

{
    my $a = attr(Q[class A4 { has $.w = 6 }]);
    nok $a.gist.contains('traits'), 'a default alone shows no traits in the gist';
    is $a.traits.elems, 1, 'but .traits answers its WillBuild';
}

is attr(Q[class A5 { has $.v }]).traits.elems, 0, 'a plain attribute has no traits';

# Write direction.
is EVAL(Q[class B1 { has $.x = 7 }; B1.new.x].AST), 7, 'a default round-trips';
ok EVAL(Q[class B2 { has $.x is rw = 1 }; B2.^attributes[0].rw].AST), 'is rw round-trips';
throws-like { EVAL Q[class B3 { has $.x is required }; B3.new].AST },
    X::Attribute::Required, 'is required round-trips';
is EVAL(Q[class B4 { has Int $.n is required }; B4.new(n => 3).n].AST), 3,
    'and still accepts the argument';
