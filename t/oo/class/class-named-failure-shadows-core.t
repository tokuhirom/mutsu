use Test;
plan 8;

# A top-level `class Failure` shadows the built-in, like `my class Failure` (#13141).
class Failure { has $.file; has $.message; method shout { "!{$!message}" } }

my $f = Failure.new(:file('a.raku'), :message('x'));
is $f.^name, 'Failure', 'the user class constructs';
is $f.file, 'a.raku', 'its own accessor answers';
is $f.message, 'x', 'its message is the attribute, not the core Failure message';
is $f.shout, '!x', 'its own method runs';
ok $f ~~ Failure, 'it smartmatches its own class';
ok $f.defined, 'and is an ordinary defined object';

# The failure `fail` makes is still the built-in one.
sub failing { fail "boom" }
my $r = failing();
is $r.^name, 'Failure', 'fail still returns the core Failure';
nok $r ~~ Failure, 'which is not an instance of the user class';
$r.so;
