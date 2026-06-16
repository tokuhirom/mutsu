# Hash.push / Hash.append must flatten a Seq (or Slip) of Pairs the same way it
# flattens an Array of Pairs. A Seq operand (e.g. from `%h.push: %x.invert`) used
# to fall through to the alternating key/value arm, stringifying the whole Seq
# into a single mangled key.
use Test;

plan 5;

# Seq of pairs (literal .Seq).
{
    my %h = a => 1;
    %h.push: (b => 2, c => 3).Seq;
    is-deeply %h, {a => 1, b => 2, c => 3}, 'push of a Seq of pairs flattens';
}

# .invert returns a Seq of pairs.
{
    my %h    = a => 1;
    my %x    = x => 'a', y => 'b';
    %h.push: %x.invert;
    is-deeply %h, {a => [1, 'x'], b => 'y'}, 'push of %h.invert (a Seq) flattens';
}

# Existing key stacks into an array (push semantics) via a Seq operand.
{
    my %h = a => 1;
    %h.push: (a => 2, a => 3).Seq;
    is-deeply %h<a>, [1, 2, 3], 'push of a Seq stacks repeated keys';
}

# append from a Seq.
{
    my %h = a => [1];
    %h.append: (a => 2, b => 3).Seq;
    is-deeply %h<a>, [1, 2], 'append of a Seq flattens into existing array';
    is-deeply %h<b>, 3,      'append of a Seq stores a fresh key as a scalar';
}
