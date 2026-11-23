use Test;

# A multi-method candidate's invocant is one of its parameters. Two candidates
# where one has the narrower invocant and the other the narrower argument
# type are incomparable, so the call is X::Multi::Ambiguous, as on rakudo
# (#18868).

plan 8;

# An added `my method` has a `Mu` invocant: `(Mu: Str)` ties `(Late: Cool)`.
class Late { multi method g(Int $x) { "int" }; multi method g(Cool $x) { "cool" } }
my $l = Late.new;
Late.^add_multi_method('g', my method g2(Str $x) { "str" });
Late.^compose;
throws-like { $l.g("a") }, X::Multi::Ambiguous, 'an added method splits with a class candidate';
is $l.g(1), 'int', 'a candidate narrower on every position still wins';

# Two candidates only.
class Two { multi method g(Cool $x) { "cool" } }
Two.^add_multi_method('g', my method g3(Str $x) { "str" });
Two.^compose;
throws-like { Two.new.g("a") }, X::Multi::Ambiguous, 'the split with a single class candidate';

# With nothing narrower in the class, the added candidate wins alone.
class Only { multi method g(Int $x) { "int" } }
Only.^add_multi_method('g', my method g4(Str $x) { "str" });
Only.^compose;
is Only.new.g("a"), 'str', 'an added candidate with no rival runs';
is Only.new.g(2), 'int', '... and the class candidate still runs';

# A subclass's candidate against its parent's.
class P { multi method f(Str $x) { "P-str" } }
class K is P { multi method f(Cool $x) { "K-cool" } }
throws-like { K.new.f("a") }, X::Multi::Ambiguous, 'a subclass invocant against a narrower parent argument';
is K.new.f(1), 'K-cool', 'an argument only the subclass candidate takes';

# The same invocant type (a smiley is no narrower type): the argument decides.
class Sm { multi method f(Str $x) { "S-str" }; multi method f(Sm:D $obj: Cool $x) { "S-cool" } }
is Sm.new.f("a"), 'S-str', 'the same invocant class leaves the argument to decide';
