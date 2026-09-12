use Test;

# A signature whose optional parameters the const-fill precompute cannot
# represent -- a default that reads an earlier parameter, an arbitrary
# expression, a container literal -- used to forfeit the positional light-call
# path on EVERY call, including the calls that supply every positional and so
# never consult the default at all (#7581). Such a call is now admitted.
#
# The point of the test is that admitting it did not change what these
# signatures BIND, in either direction: a full-arity call must not evaluate the
# default, and a short call must still get the general binder's per-call
# evaluation. Every case below is pinned against rakudo's answer.

plan 26;

# --- a default that reads an earlier parameter -------------------------------
sub reads-earlier($x, $y = $x + 1) { "$x/$y" }
is reads-earlier(1, 9), "1/9", 'full arity binds the argument';
is reads-earlier(1),    "1/2", 'omitted still evaluates the default against $x';
is reads-earlier(7),    "7/8", 'the default tracks the argument, not a constant';

# --- the default must NOT run when the call supplies the parameter -----------
my $ran = 0;
sub side($x, $y = do { $ran++; 100 }) { "$x/$y" }
is side(5, 7), "5/7", 'full arity binds the argument';
is $ran, 0,           'a supplied parameter does not evaluate its default';
is side(5), "5/100",  'an omitted parameter does evaluate it';
is $ran, 1,           'evaluated exactly once';
is side(5), "5/100",  'and again on the next omitting call';
is $ran, 2,           'evaluated per call, not memoized';

# --- a container default is a FRESH container per omitting call --------------
sub fresh($y = []) { $y.push(1); $y.elems }
is fresh(), 1, 'container default is fresh (first call)';
is fresh(), 1, 'container default is fresh (second call)';
is fresh(), 1, 'container default is fresh (third call)';
my @given = 9, 9;
is fresh(@given), 3, 'a supplied container is the caller\'s own';
is @given.elems,  3, 'and the push is visible to the caller';

# --- a zero-argument parenthesized call (carries the callsite-line marker) ----
sub zero($y = 40 + 2) { $y }
is zero(),  42, 'a parenthesized zero-arg call still reaches the default';
is zero(7), 7,  'and a supplied argument still wins';

# --- type constraints still apply on the newly-admitted path -----------------
sub typed(Int $x, Int $y = $x + 1) { $x + $y }
is typed(1, 2), 3, 'full arity type-checks and binds';
is typed(1),    3, 'omitted evaluates the default';
# Through a variable, so rakudo's compile-time "will never work" analysis does
# not reject the call outright and both implementations check it at run time --
# which is the check the newly-admitted light bind performs.
my $not-an-int = "s";
dies-ok { typed(1, $not-an-int) }, 'a full-arity argument is still type-checked';

# --- arity errors are unchanged ----------------------------------------------
# Through a capture for the same reason: a literal over-long call is a
# compile-time error in rakudo, so there would be nothing left to run.
my @three = 1, 2, 3;
dies-ok { reads-earlier(|@three) }, 'too many positionals still dies';
sub two-required($a, $b, $c = $a + $b) { "$a/$b/$c" }
my @one = (1,);
dies-ok { two-required(|@one) }, 'short of the mandatory prefix still dies';
# The case the light path itself must get right: short of FULL arity but at or
# above the mandatory prefix, which has to reach the general binder rather than
# the light bind's "Too few positionals".
is two-required(1, 2),    "1/2/3", 'the fillable tail still defaults';
is two-required(1, 2, 9), "1/2/9", 'and is overridden when supplied';

# --- a named argument is not swallowed as a positional -----------------------
# The light bind takes every argument positionally, so admitting a call to it
# must not turn `f(1, :verbose)` into a positional bind of the Pair. Pinned
# because the newly-admitted full-arity path is exactly where that could start
# happening: these routines have always reached the general binder, which
# rejects the call, and must continue to.
sub named-taker($x, $y = $x + 1) { "$x/$y" }
dies-ok { named-taker(1, :verbose) }, 'a named argument is still rejected';
is named-taker(1, 2), "1/2", 'and the ordinary full-arity call still works';

# --- the routine keeps working after being served on both paths --------------
# The positional light-call cache is keyed by NAME, so one entry serves every
# arity this file calls `mixed` with. Alternating the two arities is what would
# catch a cached full-arity entry being reused for a short call.
sub mixed($x, $y = $x * 10) { "$x/$y" }
my @seen;
for 1..3 -> $i {
    @seen.push(mixed($i));
    @seen.push(mixed($i, 0));
}
is @seen.join(','), '1/10,1/0,2/20,2/0,3/30,3/0',
    'alternating full-arity and short calls each bind correctly';
