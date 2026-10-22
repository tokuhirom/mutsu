use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 11;

my $pointy = Q[-> \t { t = 5 }].AST.statements.head.expression;
is $pointy.^name, 'RakuAST::PointyBlock', 'a sigilless pointy closure is a PointyBlock';
is $pointy.signature.^name, 'RakuAST::Signature', 'the pointy block retains its signature';
is $pointy.signature.parameters.head.target.^name,
    'RakuAST::ParameterTarget::Term', 'the parameter target records sigilless binding';
is $pointy.signature.parameters.head.target.name.gist,
    RakuAST::Name.from-identifier('t').gist, 'the target retains the term name';

is EVAL(Q[
    my @a = 1, 2, 3;
    my $b = -> \t { t = 5 };
    $b(@a[0]);
    @a.raku
].AST), '[5, 2, 3]', 'a stored sigilless pointy closure writes through an array element';

is EVAL(Q[
    my @a = 1, 2, 3;
    my &cv = -> \t { t = 6 };
    cv(@a[1]);
    @a.raku
].AST), '[1, 6, 3]', 'a code variable call writes through an array element';

is EVAL(Q[
    my @a = 1, 2, 3;
    my &cv = -> \t { t = 7 };
    &cv(@a[2]);
    @a.raku
].AST), '[1, 2, 7]', 'an ampersand call writes through an array element';

is EVAL(Q[
    my %h = a => 1;
    my $b = -> \t { t = 8 };
    $b(%h<a>);
    %h<a>
].AST), 8, 'a sigilless pointy closure writes through a hash element';

is EVAL(Q[
    my @a = 1, 2;
    my $b = -> \t { t + 1 };
    $b(@a[0])
].AST), 2, 'a sigilless pointy parameter can read its argument';

is EVAL(Q[
    my @a = 1, 2;
    (-> \t { t = 9 })(@a[1]);
    @a.raku
].AST), '[1, 9]', 'an immediate sigilless pointy closure writes through an array element';

is EVAL(Q[
    my @a = 1, 2, 3;
    my $b = -> \t { t = 5 };
    $b(@a[0]);
    @a[1]
].AST), 2, 'writing through the argument does not change a neighboring element';
