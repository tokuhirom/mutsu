use lib $?FILE.IO.parent(3).add('lib');
use Test;
use MemOpReexport;

# An operator multi a `sub EXPORT` hook hands out competes with the core
# candidates by narrowness: a `(Mu, Mu)` `∈` is wider than the core `(Mu, Any)`
# and loses, one with a narrower parameter wins (#17297).

plan 4;

is-deeply (1 ∈ [1, 2]), True, 'a (Mu, Mu) ∈ multi does not replace the core operator';
is-deeply ([1, 2] ∋ 1), True, 'nor does a (Mu, Mu) ∋ multi';
is-deeply (3 ∈ [1, 2]), False, 'the core operator answers a miss';
is (Wide.new(n => 1) ∈ [1]), 'narrow', 'a candidate narrower than the core one still wins';
