use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 10;

sub decl($source) { $source.AST.statements.tail.expression }

my $hides = decl(Q[role R1 hides Exception { }]);
isa-ok $hides.traits[0], RakuAST::Trait::Hides,
    '`hides Parent` is a Trait::Hides';
isa-ok $hides.traits[0].type, RakuAST::Type::Simple,
    'Trait::Hides holds the parent type';
is $hides.traits[0].type.name.gist,
    'RakuAST::Name.from-identifier("Exception")',
    'the hidden parent name is retained';

my $hidden = decl(Q[role R2 is Exception is hidden { }]);
isa-ok $hidden.traits[0], RakuAST::Trait::Is,
    'the role parent remains a Trait::Is';
isa-ok $hidden.traits[1], RakuAST::Trait::Is,
    '`is hidden` is represented as a Trait::Is';
is $hidden.traits[1].name.gist,
    'RakuAST::Name.from-identifier("hidden")',
    'the hidden marker is the trait name';

is EVAL(Q[
    my class C1 { }
    my class C2 is C1 { }
    my role R3 hides C2 { }
    my class C3 does R3 { }
    my class C4 is C3 { }
    C4.^mro_unhidden.map(*.^name).join(',')
].AST), 'C4,C3,C1,Any,Mu',
    'a hidden role parent survives AST conversion and lowering';

is EVAL(Q[
    my class C1 { }
    my class C2 is C1 { }
    my role R4 is C2 is hidden { }
    my role R5 { }
    my class C5 is R4 is R5 { }
    my class C6 is C5 { }
    C6.^mro_unhidden.map({ .^name.subst(/\+.*/, '') }).join(',')
].AST), 'C6,C5,C2,C1,R5,Any,Mu',
    '`is hidden` survives AST conversion and lowering';

is EVAL(Q[role R6 hides Exception { }; R6.^pun.^name].AST),
    'R6', 'the lowered hidden role can be punned';
is EVAL(Q[role R7 is Exception is hidden { }; R7.^pun.^name].AST),
    'R7', 'the lowered hidden role remains a role';
