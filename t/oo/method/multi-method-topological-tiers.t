use Test;

# Multi methods are ranked in rakudo's topological tiers over every visible
# candidate, the ones that do not bind included, as multi subs are (#12795).
# A candidate whose arguments are unrelated to its parameter types takes no
# part in the tiers. Expected values from rakudo.

plan 11;

class T {
    multi method a(Int $g, Int $e, $d = '')         { "A" }
    multi method a(Any $g, Any $e, Any $t, $d = '') { "B" }
    multi method a(Int $g, Int $e, $d = '', :$rel!) { "C" }
}
is T.a(5, 6, 1), 'B', 'a later-tier match loses to an incomparable first-tier one';
is T.a(5, 6), 'A', 'it still wins a call only it binds';
is T.a(5, 6, :rel(1)), 'C', 'the required-named candidate wins with its named';

class K {
    multi method m(Int $g, Int $e, $d = '')         { "A" }
    multi method m(Any $g, Any $e, Any $t, $d = '') { "B" }
}
throws-like { K.m(5, 6, 1) }, X::Multi::Ambiguous,
    'two arity-incomparable binders in one tier are ambiguous';

class P { multi method m(Int $g, Int $e, $d = '') { "PA" } }
class Q is P { multi method m(Any $g, Any $e, Any $t, $d = '') { "QB" } }
throws-like { Q.m(5, 6, 1) }, X::Multi::Ambiguous, 'across owners too';

class D {
    multi method m(Int $g, Int $e, $d = '') is default { "A" }
    multi method m(Any $g, Any $e, Any $t, $d = '')    { "B" }
}
is D.m(5, 6, 1), 'A', 'is default settles the ambiguity';

class Approx {
    multi method approx(Numeric $got, Numeric $expected, $desc = '') { "rel" }
    multi method approx(Numeric $got, Numeric $expected, Numeric $abs-tol, $desc = '') { "abs" }
    multi method approx(Numeric $got, Numeric $expected, $desc = '', Numeric :$rel-tol!) { "rel-tol" }
}
is Approx.approx(5, 6, 1), 'abs', 'the is-approx shape reaches the absolute tolerance';
is Approx.approx(5, 6, 'd'), 'rel', 'and the description candidate';

# A literal candidate the arguments cannot bind does not reorder the rest.
class W {
    multi method w(1, 'em')                     { 'em' }
    multi method w($freq, 'khz')                { 'khz' }
    multi method w(Numeric $num, Str:D $units)  { 'NS' }
}
is W.w(0.1, 'khz'), 'khz', 'an unrelated literal rival leaves the tiers alone';
is W.w(1, 'em'), 'em', 'the literal candidate still wins its own call';

multi sub w(1, 'em')                    { 'em' }
multi sub w($freq, 'khz')               { 'khz' }
multi sub w(Numeric $num, Str:D $units) { 'NS' }
is w(0.1, 'khz'), 'khz', 'and so for a multi sub';
