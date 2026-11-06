use Test;
use nqp;
plan 7;

# `IterationBuffer.List` is a List built over the buffer's own storage, so an
# in-place push / shift on the buffer shows through a List taken earlier.
my $b := nqp::create(IterationBuffer);
nqp::push($b, 1);
nqp::push($b, 2);
my $l = $b.List;
nqp::push($b, 3);
is $l.elems, 3, 'a List taken earlier sees a later nqp::push';
nqp::shift($b);
nqp::push($b, 4);
is $l.raku, '$(2, 3, 4)', 'and a sliding window (shift + push)';

# `nqp::clone` of a buffer owns a copy of the elements.
my $c := nqp::clone($b);
nqp::push($c, 9);
is nqp::elems($b), 3, 'a push onto the clone leaves the original alone';
is nqp::elems($c), 4, 'the clone grew';

# The sliding-window idiom of Sequence::Generator's LambdaN.
my $window := nqp::create(IterationBuffer);
nqp::push($window, $_) for 1, 1, 2, 4;
my $list := $window.List;
my @out = 1, 1, 2, 4;
for ^6 {
    my $next = $list.sum;
    @out.push: $next;
    nqp::shift($window);
    nqp::push($window, $next);
}
is-deeply @out, [1, 1, 2, 4, 8, 15, 29, 56, 108, 208], 'tetranacci through a shared List';

# Method-level push is a separate path; it must keep working.
my $m := nqp::create(IterationBuffer);
$m.push(1); $m.push(2);
is $m.elems, 2, '.push still works';
is $m.List.raku, '(1, 2)', 'and .List after it';
