use v6;
use Test;

# A multi family with a value-dependent candidate (`where`, a subset such as
# `UInt`, a literal, a value constant) cannot cache its winner per argument
# type. Its candidate gathering and ranking are still type-determined, so they
# are cached as a dispatch plan and only the bind checks run per call (#9967).
# These pin that the plan never replaces a per-call check.

plan 18;

# --- UInt: the subset predicate runs on every call ---------------------------
multi u(UInt $a, UInt $b) { "uint" }
multi u(Int $a, Int $b)   { "int" }
my @got;
for (3, 4), (-3, 4), (5, 6), (3, -4), (7, 8) -> ($a, $b) { @got.push: u($a, $b) }
is-deeply @got, [<uint int uint int uint>], 'UInt candidate re-checked per call';

# --- A where clause runs once per call, and only when reached -----------------
my $runs = 0;
multi w(Int $x where { $runs++; $x > 0 }) { "pos" }
multi w(Int $x) { "other" }
multi w(Str $x) { "str" }
is w(1), "pos", 'where accepts';
is w(-1), "other", 'where rejects, wider candidate wins';
is w(2), "pos", 'where accepts again after a rejection';
is w("s"), "str", 'a Str argument never reaches the Int where';
is $runs, 3, 'the where clause ran exactly once per Int call';

# --- Values of one type that rank differently (Inf / NaN) ---------------------
multi n(Numeric $x) { "Numeric" }
multi n(Inf)        { "Inf" }
multi n(NaN)        { "NaN" }
is n(Inf), "Inf", 'Inf candidate';
is n(NaN), "NaN", 'NaN candidate after an Inf call with the same type';
is n(1e0), "Numeric", 'a plain Num after both';
is n(NaN), "NaN", 'NaN again';

# --- Literal candidates -------------------------------------------------------
multi l(0)      { "zero" }
multi l(1)      { "one" }
multi l(Int $x) { "int $x" }
is (l(0), l(1), l(2), l(1), l(0)).join(","), "zero,one,int 2,one,zero",
    'literal candidates alternate with the typed one';

# --- An operator with a UInt candidate that defers with callsame --------------
{
    my $*m = 7;
    multi infix:<+>(UInt $a, UInt $b --> UInt) { callsame() mod $*m }
    is 5 + 4, 2, 'user UInt candidate reduces modulo';
    is -5 + 4, -1, 'a negative operand falls through to the core candidate';
    is 6 + 6, 5, 'and the user candidate still applies afterwards';
}

# --- A value-constant parameter does not ask an unrelated argument's WHICH ----
my $which-calls = 0;
class P {
    has $.v;
    method WHICH { $which-calls++; "P|$!v" }
}
my constant K = P.new(v => 1);
multi k(Int $n, K) { "K" }
multi k(Int $n, Int $m) { "ints" }
is k(1, 2), "ints", 'an Int argument binds the Int candidate';
is $which-calls, 0, 'the constant\'s WHICH is not consulted for an Int argument';
is k(1, K), "K", 'the constant itself still binds';
is k(1, P.new(v => 1)), "K", 'an equal-WHICH object binds the constant candidate';
