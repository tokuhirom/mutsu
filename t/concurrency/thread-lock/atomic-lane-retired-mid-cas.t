use Test;

# #9921 / ADR-0062 "Not addressed" (2): a `cas` whose legacy name-keyed lane is
# retired by another thread *during* the `cas` must not write the retired slot.
# The lane is keyed by bare name process-wide, so an unrelated `my $x` in a
# worker retires it; the swap must still land in the `cas`ed variable.

plan 4;

class Node { has $.n }

my $go = Channel.new;
my $done = Channel.new;
my $writer = start {
    $go.receive;
    my $x = 1;
    $x = 2;
    $done.send(1);
    $x
};

# A type-object-valued scalar takes the legacy lane (not a shared cell).
my Node $x;
my $first = True;
my $r = cas $x, -> $v {
    if $first { $first = False; $go.send(1); $done.receive }
    Node.new(n => ($v.defined ?? $v.n !! 0) + 1)
};
is await($writer), 2, 'the unrelated same-spelled binding is untouched';
is $r.n, 1, 'cas returns the swapped-in value';
ok $x ~~ Node:D, 'the swap reached the variable, not a retired lane slot';

cas $x, -> $v { Node.new(n => $v.n + 1) };
is $x.n, 2, 'a later cas continues from the swapped value';
