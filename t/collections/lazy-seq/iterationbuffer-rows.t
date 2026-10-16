use Test;

# `IterationBuffer`'s methods are rows of the method table, found by their
# owner (ADR-11276 §9.121).

plan 12;

my $b = IterationBuffer.new;
$b.push(1);
$b.push(2);
$b.unshift(0);
is $b.elems, 3, 'push, unshift, elems';
is $b.List.raku, '(0, 1, 2)', 'List';
is $b.AT-POS(2), 2, 'AT-POS';
is $b.AT-POS(9).^name, 'Mu', 'AT-POS past the end is Mu';
is $b.BIND-POS(4, 'x'), 'x', 'BIND-POS answers the value';
is $b.List.raku, '(0, 1, 2, Mu, "x")', 'BIND-POS grows the buffer with Mu';

my $other = IterationBuffer.new;
$other.push(-1);
my $tail = IterationBuffer.new;
$tail.push(7);
$tail.push(8);
$b.append($tail);
$b.prepend($other);
is $b.Seq.raku, '(-1, 0, 1, 2, Mu, "x", 7, 8).Seq', 'append, prepend; Seq';
is $b.Slip.raku, 'slip(-1, 0, 1, 2, Mu, "x", 7, 8)', 'Slip';
is $b.iterator.pull-one, -1, 'iterator';
$b.clear;
is $b.elems, 0, 'clear';

ok IterationBuffer.^can('push'), '.^can sees push';

# A subclass's own method overrides the buffer's.
class Frozen is IterationBuffer {
    method push(|) { die 'frozen' }
}
my $f = Frozen.new;
throws-like { $f.push(1) }, X::AdHoc, message => 'frozen',
    "a subclass's push overrides the row";
