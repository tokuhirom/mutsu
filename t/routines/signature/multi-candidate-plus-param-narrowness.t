use Test;

# A single-argument slurpy (`+@a`) is a slurpy parameter: its implied
# Positional constraint is no narrowness, so a candidate without it wins a call
# both can bind, and `nextwith` still reaches it. (Found with Red's
# `multi method refresh(\model) { nextwith model, |@columns }` followed by
# `multi method refresh(\model, +@attrs)`, which never ran the first candidate.)

plan 11;

multi sub z($m) { 'one' }
multi sub z($m, +@a) { 'plus ' ~ @a.join(',') }
is z(5), 'one', 'multi sub: the fixed-arity candidate wins z(5)';
is z(5, 6), 'plus 6', 'multi sub: the +@a candidate takes more arguments';

multi sub w($m, +@a) { 'plus' }
multi sub w($m) { 'one' }
is w(5), 'one', 'declaration order does not matter';

multi sub y($m) { 'one' }
multi sub y($m, *@a) { 'star' }
is y(5), 'one', '*@a: the same';

class C {
    multi method u($m) { 'one' }
    multi method u($m, +@a) { 'plus' }
}
is C.new.u(5), 'one', 'multi method: the fixed-arity candidate wins';

multi sub n(\m) { nextwith m, 1, 2 }
multi sub n(\m, +@a) { 'n: ' ~ @a.join(',') }
is n(5), 'n: 1,2', 'nextwith reaches the +@a candidate with its arguments';

class K {
    multi method p(\m) { nextwith m, 1, 2 }
    multi method p(\m, +@a) { 'p: ' ~ @a.join(',') }
    multi method q(\m) { callwith m, |<x y> }
    multi method q(\m, +@a) { 'q: ' ~ @a.join(',') }
    multi method r(\m) { samewith m, |(3, 4) }
    multi method r(\m, +@a) { 'r: ' ~ @a.join(',') }
    multi method s(\m) { nextwith m, [7, 8] }
    multi method s(\m, +@a) { 's: ' ~ @a.join(',') }
}
is K.new.p(5), 'p: 1,2', 'method nextwith';
is K.new.q(5), 'q: x,y', 'method callwith';
is K.new.r(5), 'r: 3,4', 'method samewith';
is K.new.s(5), 's: 7,8', 'single-argument rule under nextwith';

role R {
    multi method refresh(Int:D \model) { nextwith model, |<a b> }
    multi method refresh(Int:D \model, +@attrs) { 'role ' ~ @attrs.join(',') }
}
class UsesR does R { }
is UsesR.new.refresh(7), 'role a,b', 'role multi methods';

done-testing;
