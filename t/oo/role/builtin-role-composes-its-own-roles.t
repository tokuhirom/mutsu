use Test;

plan 12;

# A built-in role composes roles of its own. `Real does Numeric`, so a class
# that only says `does Real` is a Numeric too.
class Fixed does Real {
    has Int $.hundredths;
    method Bridge() { $!hundredths.Bridge / 100.Bridge }
}

my $one = Fixed.new(hundredths => 100);

ok $one ~~ Real, 'the instance does the role it named';
ok $one ~~ Numeric, 'and the role that role composes';
ok Fixed ~~ Real, 'the type object does the role it named';
ok Fixed ~~ Numeric, 'and the role that role composes';
nok $one ~~ Cool, 'Real does not drag Cool in';

ok Real ~~ Numeric, 'the built-in role itself does its parent';
ok Setty ~~ QuantHash, 'Setty does QuantHash';
ok Baggy ~~ QuantHash, 'Baggy does QuantHash';
ok Mixy ~~ Baggy, 'Mixy does Baggy';
ok Mixy ~~ QuantHash, 'and transitively QuantHash';

# The relation is what a `Numeric` parameter constraint is checked against, and
# getting it wrong silently diverted Test.rakumod's `is-approx(Numeric, Numeric)`
# candidates (roast/S32-num/real-bridge.t).
sub takes-numeric(Numeric $n) { 'matched' }
is takes-numeric($one), 'matched', 'a Numeric parameter accepts it';

sub takes-real(Real $r) { 'matched' }
is takes-real($one), 'matched', 'and so does a Real parameter';
