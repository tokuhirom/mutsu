use Test;

# The `multi`-existence probes and the candidate gather no longer walk every
# registered routine key comparing it against a `format!`ed `"{pkg}::{name}/"`
# prefix; they ask the base-name key index for the handful of keys that could
# match and compare the pieces in place (#8300). These are the name and package
# shapes where an off-by-one in that comparison, or a wrong base-name bucket,
# would send a call to the wrong candidate -- or to none at all.

plan 15;

# --- a name that is a proper prefix of another name -------------------------
multi plain(Int $x) { "plain-Int" }
multi plain(Str $x) { "plain-Str" }
multi plainer(Int $x) { "plainer-Int" }

is plain(1), 'plain-Int', 'multi resolves on the exact base name';
is plain('a'), 'plain-Str', 'and on the other candidate at the same arity';
is plainer(1), 'plainer-Int', 'a name that merely EXTENDS another is separate';

# --- an operator name carrying its own `/` ----------------------------------
# `infix:</>`'s key is `GLOBAL::infix:</>/2`: the `/` inside the name is not
# followed by a digit, so only the arity `/2` may be read as the suffix.
class Frac { has $.n }
multi sub infix:</>(Frac $a, Frac $b) { "frac-div" }
is (Frac.new(n => 1) / Frac.new(n => 2)), 'frac-div',
    'a multi operator whose name contains a slash still dispatches';
is (6 / 3), 2, 'and the core numeric candidate is untouched';

# --- enclosing-package candidates -------------------------------------------
module Outer {
    multi sub helper(Int $x) { "outer-Int" }
    multi sub helper(Str $x) { "outer-Str" }
    module Inner {
        our sub reach-out() { (helper(1), helper('a')).join('|') }
    }
    our sub reach-here() { (helper(2), helper('b')).join('|') }
}

is Outer::reach-here(), 'outer-Int|outer-Str',
    'a bare multi call finds its own package candidates';
is Outer::Inner::reach-out(), 'outer-Int|outer-Str',
    'and a nested package reaches the enclosing package candidates';

# --- a package-qualified call ------------------------------------------------
module Q {
    our proto pick($) {*}
    multi sub pick(Int $x) { "q-Int" }
    multi sub pick(Str $x) { "q-Str" }
}
is Q::pick(3), 'q-Int', 'a package-qualified multi call resolves';
is Q::pick('c'), 'q-Str', 'and picks the other candidate by type';

# --- a lexical `my sub` re-declared on every call ---------------------------
# This is the shape that churns the registry: entering the routine registers
# the inner sub and leaving it removes it again, invalidating every name-keyed
# dispatch cache. The answer must not depend on how many times it has run.
sub declares-an-inner-sub($n) {
    my sub inner(Int $x) { $x * 2 }
    inner($n);
}
is declares-an-inner-sub(1), 2, 'a per-call lexical sub resolves the first time';
is declares-an-inner-sub(2), 4, 'and the second';
is (^5).map({ declares-an-inner-sub($_) }).join(','), '0,2,4,6,8',
    'and stays correct across repeated registry churn';

# The same, with the inner routine a multi: the candidate gather runs over the
# index, and the index is what the churn drops.
sub declares-inner-multis($v) {
    multi sub pickety(Int $x) { "i$x" }
    multi sub pickety(Str $x) { "s$x" }
    pickety($v);
}
is declares-inner-multis(7), 'i7', 'a per-call lexical multi resolves';
is declares-inner-multis('z'), 'sz', 'on both candidates';
is (1, 'a', 2, 'b').map({ declares-inner-multis($_) }).join(','), 'i1,sa,i2,sb',
    'and keeps picking correctly as the registry churns';
