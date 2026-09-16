use Test;
use lib 't/lib';
use ArgTraitFixture;

# #8560: an *imported* `trait_mod:<is>` on a parameter carrying its own
# argument (`is option<!>`, `is option(...)`) never dispatched, because
# `check_param_custom_traits` always handed the candidate a hardcoded `True`
# instead of the real argument -- App::Prove6's `is option<!>` (via
# Getopt::Long) reduces to exactly this: two candidates on the same external
# name that only differ by the *value*'s type (`Argument` vs `Str`), and
# neither ever accepts `True`. The result was always the generic "unknown
# trait" error, even though a real candidate matched the actual value.
#
# ArgTraitFixture mirrors that shape without depending on Getopt::Long
# itself: `is tag<hello>` (a word-quote argument) must reach dispatch as the
# Str "hello" and select the `Str:D` candidate; `is tag(Marker.new)` (a
# parenthesized argument) must reach dispatch as a `Marker` instance and
# select the `Marker:D` candidate.

plan 3;

sub demo1(:$x is tag<hello>) { }
sub demo2(:$y is tag(Marker.new)) { }

ok &demo1.signature.params[0] ~~ FromStr,
    'a word-quote trait argument (is tag<hello>) reaches dispatch as a Str and selects the Str:D candidate';
ok &demo2.signature.params[0] ~~ FromMarker,
    'a parenthesized trait argument (is tag(Marker.new)) reaches dispatch as the real value and selects the Marker:D candidate';

# A real error raised from inside a *matched* candidate's body must still
# propagate as itself -- not be swallowed and reported as a generic "unknown
# trait", which would make debugging a real trait_mod bug indistinguishable
# from a typo'd trait name.
throws-like { EVAL 'sub oops(:$z is strict-tag<bad>) { }' }, Exception,
    'a real error from a matched candidate body propagates, not masked as "unknown trait"',
    message => /'bad strict-tag value'/;
