use Test;

# `Metamodel::GrammarHOW is Metamodel::ClassHOW`, so a grammar's HOW takes a
# trait's `when Metamodel::ClassHOW` arm (AttrX::Mooish via Config::BINDish,
# #14154). No other built-in metaclass is a subclass of another.

plan 7;

grammar G { token TOP { .* } }
class C { }

ok G.HOW ~~ Metamodel::ClassHOW, 'a GrammarHOW is a ClassHOW';
ok G.HOW ~~ Metamodel::GrammarHOW, 'a GrammarHOW is a GrammarHOW';
is G.HOW.^name, 'Perl6::Metamodel::GrammarHOW', 'its name is unchanged';
nok C.HOW ~~ Metamodel::GrammarHOW, 'a ClassHOW is no GrammarHOW';
ok C.HOW ~~ Metamodel::ClassHOW, 'a ClassHOW is a ClassHOW';
nok G.HOW ~~ Metamodel::EnumHOW, 'a GrammarHOW is no EnumHOW';

my $arm = do given G.HOW {
    when Metamodel::ParametricRoleHOW { 'role' }
    when Metamodel::ClassHOW { 'class' }
    default { 'other' }
};
is $arm, 'class', 'a grammar takes the ClassHOW arm';
