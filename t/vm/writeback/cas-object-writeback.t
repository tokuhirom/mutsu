use Test;

plan 2;

class CasObjectWriteback::Node {
    has $.value;
    has CasObjectWriteback::Node $.next;
}

my $tail = CasObjectWriteback::Node.new(value => 'tail');
my $head = CasObjectWriteback::Node.new(value => 'head', next => $tail);
my $taken;

# Regression distilled from Concurrent::Stack's object-valued CAS callback.
cas $head, -> $current {
    $taken = $current;
    $current.next;
};

is $taken.value, 'head', 'CAS callback writes an object to its captured lexical';
is $head.value, 'tail', 'the object returned by CAS replaces the head';
