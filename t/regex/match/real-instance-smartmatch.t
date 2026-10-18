use Test;

# Smartmatch against an instance of a user class composing `Real` is
# `Numeric.ACCEPTS`, `a == self`, on either side, so it compares through the
# class's `Bridge` (#13894).

plan 11;

class R does Real {
    has $.n;
    method Bridge { $!n.Num }
}

ok 3 ~~ R.new(n => 3), 'a number against an instance (R.ACCEPTS(3))';
nok 4 ~~ R.new(n => 3), '... which compares the values';
ok R.new(n => 3) ~~ 3, 'an instance against a number (Int.ACCEPTS(R))';
nok R.new(n => 3) ~~ 3.5, '... a Rat too';
ok R.new(n => 3).ACCEPTS(3), 'an explicit ACCEPTS';
ok R.new(n => 3) ~~ R.new(n => 3), 'two equal instances';
nok R.new(n => 3) ~~ R.new(n => 4), 'two different instances';
ok '3' ~~ R.new(n => 3), 'a numeric string topic';
ok R.new(n => 3) ~~ R, 'the type check is unchanged';

my $seen;
given R.new(n => 2) {
    when 1 { $seen = 'one' }
    when 2 { $seen = 'two' }
}
is $seen, 'two', 'given/when';

class Plain { has $.n }
nok Plain.new(n => 3) ~~ Plain.new(n => 3), 'a plain class keeps identity';
