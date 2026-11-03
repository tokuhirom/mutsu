use Test;

plan 5;

# `Metamodel::SubsetHOW.new_type(:refinement($pred))` calls the predicate when
# it is held in a variable, as when it is written inline (#15349; Kind builds
# its subsets this way).
class A {}
my $calls = 0;

my $pred = -> Mu $t { $calls++; $t.HOW ~~ Metamodel::ClassHOW };
my $held = Metamodel::SubsetHOW.new_type(:name<HeldRefinement>, :refinee(Mu), :refinement($pred));
ok A ~~ $held, 'a refinement held in a variable accepts a matching topic';
is $calls, 1, 'the predicate was called';

my $inline = Metamodel::SubsetHOW.new_type(:name<InlineRefinement>, :refinee(Mu),
    :refinement(-> Mu $t { $t.HOW ~~ Metamodel::ClassHOW }));
ok A ~~ $inline, 'the inline form still works';

my $rejecting = -> Mu $t { False };
my $no = Metamodel::SubsetHOW.new_type(:name<RejectingRefinement>, :refinee(Mu), :refinement($rejecting));
nok A ~~ $no, 'a held refinement that rejects rejects';

my $sub = anon sub accepts(Mu $t) { $t.HOW ~~ Metamodel::ClassHOW };
my $viasub = Metamodel::SubsetHOW.new_type(:name<SubRefinement>, :refinee(Mu), :refinement($sub));
ok A ~~ $viasub, 'a held anon sub works too';
