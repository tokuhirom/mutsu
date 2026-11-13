use Test;

# A `return` in a proto's CATCH, for an exception its `{*}` candidate threw,
# returns from the proto -- not from the candidate, which shares its name.

plan 6;

class X::R is Exception { method message { "r" } }

proto f(|) { {*}; CATCH { when X::R { return False } }; True }
multi f($x) { die X::R.new if $x; 1 }
is f(1), False, 'return in CATCH returns from the proto';
is f(0), True, 'no exception: the proto runs to its end';

proto k(|) { my $r = {*}; CATCH { when X::R { return 6 } }; "after" }
multi k($x) { die X::R.new }
is k(1), 6, 'the statement after {*} is not run';

my @order;
proto m(|) { {*}; CATCH { when X::R { @order.push: 'catch'; return 7 } }; 'after' }
multi m($x) { LEAVE @order.push: 'leave'; die X::R.new }
is m(1), 7, 'return value';
is @order.join(','), 'catch,leave', 'CATCH runs before the candidate unwinds';

module PC {
    our proto MAIN(|) {
        {*}
        CATCH { when X::R { return False } }
        True;
    }
    multi MAIN($q) { die X::R.new }
}
is PC::MAIN('x'), False, 'a qualified call of an our proto';
