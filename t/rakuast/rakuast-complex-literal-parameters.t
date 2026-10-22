use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 17;

sub parameter($source) {
    $source.AST.statements[0].expression.signature.parameters[0]
}

my $complex = parameter(Q[multi sub f(<1+2i>) { 1 }]);
is $complex.type.name.canonicalize, 'Complex', 'a Complex parameter has its value type';
is $complex.value.^name, 'Complex', 'its value stays Complex';
is $complex.value.raku, '<1+2i>', 'its exact value survives';
nok $complex.target.defined, 'a literal parameter has no target';
nok $complex.optional, 'a literal parameter is not optional';

my $fake = Q[:(<1+2i>)].AST.statements[0].expression.signature.parameters[0];
is $fake.type.name.canonicalize, 'Complex', 'a signature literal uses the same type';
is $fake.value.raku, '<1+2i>', 'a signature literal keeps the value';
my $fake-rational = Q[:(<1/999999999999999999999>)].AST.statements[0].expression.signature.parameters[0];
is $fake-rational.type.name.canonicalize, 'Rat', 'a large rational signature literal has the Rat type';

my $large = parameter(Q[sub f(<1/999999999999999999999>) { }]);
is $large.type.name.canonicalize, 'Rat', 'a large rational parameter is Rat';
is $large.value.^name, 'Rat', 'a large rational keeps its Raku type';
my $decimal = parameter(Q[sub f(0.12345678901234567890) { }]);
is $decimal.type.name.canonicalize, 'Rat', 'a long decimal parameter is Rat';
is $decimal.value.^name, 'Rat', 'the long decimal stays rational';

my $pointy = Q[my $f = -> <−1−2i> { 1 }].AST.statements[0].expression.initializer.expression.signature.parameters[0];
is $pointy.type.name.canonicalize, 'Complex', 'a pointy block uses the same literal type';
is $pointy.value.raku, '<-1-2i>', 'its signed Complex value survives';
is EVAL(Q[my $f = -> <−1−2i> { 'exact' }; $f(<−1−2i>)].AST), 'exact',
    'a round-tripped pointy Complex parameter matches its literal';

my $dispatch = Q[multi sub f(<1+2i>) { 'exact' }; multi sub f(Complex $x) { 'other' }; f(1+2i) ~ f(1+3i)];
is EVAL($dispatch.AST), 'exactother', 'a round-tripped Complex parameter dispatches by value';
my $rational = Q[multi sub r(<1/999999999999999999999>) { 'exact' }; multi sub r(Rat $x) { 'other' }; r(<1/999999999999999999999>) ~ r(1/2)];
is EVAL($rational.AST), 'exactother', 'a round-tripped large rational dispatches by value';
