use v6;
use Test;
use nqp;

# Regression (#9419, lizmat's ForwardIterables): the Iterator `push-*` methods
# must append to an IterationBuffer target, not only to an Array.
# `@iterables.iterator.push-all(my $b := nqp::create(IterationBuffer))` left
# the buffer empty.

plan 7;

my $c := IterationBuffer.new;
(1, 2).iterator.push-all($c);
is $c.elems, 2, 'push-all from a list iterator into IterationBuffer.new';

my @a = 1, 2;
@a.iterator.push-all(my $b := nqp::create(IterationBuffer));
is nqp::elems($b), 2, 'push-all into nqp::create(IterationBuffer)';
is $b.List, (1, 2), '... in order';

my $r := IterationBuffer.new;
(1..3).iterator.push-all($r);
is $r.elems, 3, 'push-all from a Range iterator';

my $e := IterationBuffer.new;
(1, 2).iterator.push-exactly($e, 1);
is $e.elems, 1, 'push-exactly into an IterationBuffer';

my $s := IterationBuffer.new;
(1, 1, 2, 3, 3).squish.iterator.push-all($s);
is $s.List, (1, 2, 3), 'push-all from a squish iterator';

my @t;
(1, 2, 3).iterator.push-all(@t);
is @t, [1, 2, 3], 'an Array target still works';
