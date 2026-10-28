use Test;

# GH-15177 (Red 0.2.7 t/70-submodel): `Metamodel::SubsetHOW.new_type` mints a
# real subset from :refinee / :refinement, and `(Metamodel::SubsetHOW but Role)`
# keeps the role on the minted type's HOW.
plan 12;

my \Even = Metamodel::SubsetHOW.new_type(:name<Even>, :refinee(Int), :refinement(* %% 2));
ok 4 ~~ Even, 'a value the refinement accepts matches the minted subset';
nok 3 ~~ Even, 'a value it rejects does not';
nok "x" ~~ Even, 'a value outside the refinee does not';
is Even.^refinee.^name, 'Int', '.^refinee is the base type';
is Even.HOW.^name, 'Perl6::Metamodel::SubsetHOW', 'the HOW is a SubsetHOW';

class P { has $.role }
my \Adm = Metamodel::SubsetHOW.new_type(:name<Adm>, :refinee(P), :refinement(*.role eq "admin"));
ok P.new(role => "admin") ~~ Adm, 'a class refinee';
nok P.new(role => "user") ~~ Adm, 'rejected by the refinement';

role Extra { method all(\model, |c) { "all-called(" ~ model.^refinee.^name ~ ")" } }
my \Mixed = (Metamodel::SubsetHOW but Extra).new_type(:name<Mixed>, :refinee(P), :refinement(*.role eq "admin"));
is Mixed.HOW.^name, 'Perl6::Metamodel::SubsetHOW+{Extra}', 'the mixin is part of the HOW';
is Mixed.^all, 'all-called(P)', 'a role method answers ^method on the minted type';
is Mixed.^refinee.^name, 'P', 'native metamethods still answer';
ok P.new(role => "admin") ~~ Mixed, 'the mixed-in subset still matches';
nok P.new(role => "x") ~~ Mixed, 'and still rejects';
