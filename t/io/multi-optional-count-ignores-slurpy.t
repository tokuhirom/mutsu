use Test;

# The optional-positional tie-break of multi sub dispatch counts neither a
# slurpy nor anything in a candidate that needs a named bind check, the same
# rule multi methods use (#12774).

plan 7;

multi sub j($a?, $c?, *%h where .elems == 0) { "Y" }
multi sub j($b?, :$x) { "X" }
is j(1), 'Y', 'a slurpy-hash where candidate ties a named one; first declared wins';

multi sub n($a?, *@r where .elems == 0) { "Y" }
multi sub n($b?, *@s, :$x) { "X" }
is n(1), 'Y', 'a slurpy-array where candidate ties a named one; first declared wins';

multi sub f(Int $a) { "req" }
multi sub f(*@a) { "slurpy" }
is f(1), 'req', 'a required positional still beats a slurpy';

multi sub g($a, $b?) { "opt" }
multi sub g($a) { "req" }
is g(1), 'req', 'fewer optional positionals still wins';

multi sub h($x = 1, :$a) { "A" }
multi sub h(:$b) { "B" }
is h(), 'A', 'candidates with named params tie; first declared wins';

multi sub k(*@a) { "s" }
multi sub k($a?) { "o" }
is k(), 'o', 'an optional positional beats a slurpy with no argument';
is k(1), 'o', 'an optional positional beats a slurpy with one argument';
