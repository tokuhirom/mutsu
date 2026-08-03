use v6;
use MONKEY-SEE-NO-EVAL;
use Test;

# Naming the class is only half of a typed exception: `throws-like …,
# X::Syntax::Missing, what => '"while" or "until"'` reads the ATTRIBUTE, and the
# `"X::Type: text"` parse-error message convention preserves only the class. So
# these matched the class and then died on `No such method 'what'`, aborting the
# whole file.
#
# X::Syntax::Missing derives `what` from its own message, which rakudo spells as
# `Missing {what}` -- so the two cannot disagree. X::UnitScope::* carries it
# explicitly from the raise site, and a SOFT parse diagnosis now forwards its
# structured exception the way a fatal one already did.
#
# The assertions read `.what` off the caught exception rather than going through
# `throws-like`'s named matchers: mutsu's own native `throws-like` does not
# check those, so a matcher-based pin would pass without the fix.

plan 9;

sub caught($code) {
    my $e;
    { EVAL $code; CATCH { default { $e = $_ } } }
    $e
}

my $repeat = caught 'repeat { 1 }';
isa-ok $repeat, X::Syntax::Missing, 'repeat without while/until is X::Syntax::Missing';
is $repeat.what, '"while" or "until"', '...and carries .what';
is $repeat.message, 'Missing "while" or "until"', '...with rakudo\'s own message';

my $block = caught 'if 1; 2';
isa-ok $block, X::Syntax::Missing, 'a missing block is X::Syntax::Missing';
is $block.what, 'block', '...and carries what => "block"';

my $late = caught "module AtBeginning \{\}\nsub MAIN;";
isa-ok $late, X::UnitScope::TooLate, 'a too-late unit-scoped sub is X::UnitScope::TooLate';
is $late.what, 'sub', '...and carries what => "sub"';

my $sub-scope = caught '{ sub MAIN; }';
is $sub-scope.what, 'sub', 'a unit-scoped sub in a subscope carries what => "sub"';

# `repeat ... while` itself is untouched.
my $n = 0;
repeat { $n++ } while $n < 3;
is $n, 3, 'repeat/while still runs';
