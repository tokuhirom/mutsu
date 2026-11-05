use Test;

# `Metamodel::SubsetHOW.new_type(:$refinee)` with the base held in a variable
# (Kind::Subset::Parametric's `:$refinee`): the Scalar must not become the
# base type's name.

plan 6;

my $t = Metamodel::SubsetHOW.new_type(:name<X>, :refinee(Numeric), :refinement({ $_ > 0 }));
is $t.^refinee.raku, 'Numeric', 'a type object literal';

my $r = Numeric;
my $t2 = Metamodel::SubsetHOW.new_type(:name<Y>, :refinee($r), :refinement({ $_ > 0 }));
is $t2.^refinee.raku, 'Numeric', 'the base in a scalar variable';

my $r3 := Numeric;
my $t3 = Metamodel::SubsetHOW.new_type(:name<Z>, :refinee($r3), :refinement({ $_ > 0 }));
ok $t3.^refinee =:= Numeric, 'a bound scalar';

subset SL of Numeric where 0..9;
my $r4 := SL.HOW.refinee(SL);
my $t4 = Metamodel::SubsetHOW.new_type(:name<W>, :refinee($r4), :refinement({ $_ > 0 }));
ok $t4.^refinee =:= SL.^refinee, 'the refinee of another subset';
ok 5 ~~ $t2, 'the new subset accepts a matching value';
nok -5 ~~ $t2, 'and rejects the others';

done-testing;
