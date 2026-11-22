use Test;

# A List's `pick` coerces its count to `UInt`, so a negative count dies with
# X::OutOfRange, as in rakudo. A Range, Set or Bag answers an empty pick.

plan 7;

throws-like { <a b c>.pick(-1) }, X::OutOfRange,
    message => 'Coercion to UInt out of range. Is: -1, should be in 0..^Inf', 'a List';
throws-like { [1, 2].pick(-1.5) }, X::OutOfRange,
    message => 'Coercion to UInt out of range. Is: -1, should be in 0..^Inf', 'an Array, a fractional count';
throws-like { my %h = a => 1; %h.pick(-1) }, X::OutOfRange, 'a Hash';
is-deeply (1..5).pick(-2).List, (), 'a Range picks nothing';
is-deeply set(1, 2).pick(-1).List, (), 'a Set picks nothing';
is-deeply <a b c>.pick(0).List, (), 'a zero count picks nothing';
is <a b c>.pick(2).elems, 2, 'a positive count still picks';
