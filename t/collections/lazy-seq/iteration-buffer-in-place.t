use Test;

# IterationBuffer's methods edit the buffer in place, like the nqp list ops:
# a List taken from the buffer shares its storage (Rakudo's List keeps the
# buffer as its `$!reified`), and push/elems/AT-POS no longer copy the
# whole buffer per call. Found via Concurrent::PriorityQueue, whose `.List`
# pushes one Pair per element onto a buffer and was quadratic (#15786).

plan 12;

my $ib := IterationBuffer.new;
$ib.push(1);
$ib.push(2);
is $ib.push(3), 3, 'push answers the value pushed';

my $l = $ib.List;
$ib.push(4);
is-deeply $l, (1, 2, 3, 4), 'a List taken earlier sees a later push';
is $ib.elems, 4, 'elems';
is $ib.AT-POS(1), 2, 'AT-POS';
is $ib.AT-POS(-1), 4, 'AT-POS with a negative index counts from the end';
ok $ib.AT-POS(10) =:= Mu, 'AT-POS past the end is Mu';

$ib.BIND-POS(6, 'x');
is $ib.List.raku, '(1, 2, 3, 4, Mu, Mu, "x")', 'BIND-POS past the end fills with Mu';
$ib.BIND-POS(-1, 'y');
is $ib.AT-POS(6), 'y', 'BIND-POS with a negative index counts from the end';

my $o := IterationBuffer.new;
$o.push('a');
$o.push('b');
$ib.unshift(0);
$ib.append($o);
$ib.prepend($o);
is-deeply $ib.List.head(4), ('a', 'b', 0, 1), 'unshift and prepend';
is $ib.elems, 12, 'append and prepend add the other buffer\'s elements';

my $taken = $ib.List;
$ib.clear;
is $taken.elems, 0, 'clear empties the shared storage';

my $big := IterationBuffer.new;
$big.push(Pair.new($_, $_)) for ^5000;
is $big.List.map(*.key).sum, (^5000).sum, 'a long run of pushes keeps every element';
