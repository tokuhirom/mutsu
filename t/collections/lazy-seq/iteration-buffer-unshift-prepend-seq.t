use nqp;
use Test;

# #17658: `IterationBuffer.unshift` / `.prepend` insert at the front of the
# buffer's own storage through its head offset (amortized O(a), as MoarVM's
# VMArray), and `.Seq` iterates that storage. Expected values from rakudo.

plan 9;

my $ib := IterationBuffer.new;
$ib.push($_) for 1..3;
$ib.unshift('a');
is $ib.List, ('a', 1, 2, 3), 'unshift puts the value first';
my $front := IterationBuffer.new;
$front.push('x');
$front.push('y');
$ib.prepend($front);
is $ib.List, ('x', 'y', 'a', 1, 2, 3), 'prepend keeps the order of the other buffer';
is $ib.elems, 6, 'elems counts both';
is nqp::atpos($ib, 0), 'x', 'nqp::atpos sees the front';

my $l := $ib.List;
$ib.unshift('b');
is $l.elems, 7, 'a List taken earlier sees the unshift (shared storage)';

my $e := IterationBuffer.new;
$e.unshift(1);
my $two := IterationBuffer.new;
$two.push(2);
$two.push(3);
$e.prepend($two);
is $e.List, (2, 3, 1), 'an empty buffer takes unshift and prepend';

# Many front inserts, then the buffer still reads in order.
my $m := IterationBuffer.new;
$m.unshift($_) for ^1000;
is nqp::elems($m), 1000, 'a thousand unshifts';
is nqp::atpos($m, 0), 999, 'the last unshift is first';

is $ib.Seq.List, ('b', 'x', 'y', 'a', 1, 2, 3), '.Seq gives the elements';
