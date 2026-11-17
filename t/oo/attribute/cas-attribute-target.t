use Test;

# An atomic op on an attribute (`cas $!head, &code`, `$!n⚛++`) targets the
# invocant's attribute cell, whatever lexicals the method has in scope: a
# lexical holding the same object or spelling the same name is not an alias of
# the attribute (#11482). Every expectation was checked against Rakudo.

plan 8;

class Node { has $.value; has Node $.next }

class Stack {
    has Node $!head;
    has atomicint $.elems;
    method push($value) {
        my $before = $!head;          # holds the very object the cas replaces
        my $spelled = '!head';
        cas $!head, -> $next { Node.new(:$value, :$next) };
        $!elems⚛++;
        ($before, $spelled)
    }
    method pop() {
        my $taken;
        cas $!head, -> $current {
            fail 'empty' without $current;
            $taken = $current.value;
            $current.next
        };
        $!elems⚛--;
        $taken
    }
    method values() {
        my @out;
        my $n = $!head;
        while $n { @out.push: $n.value; $n = $n.next }
        @out
    }
}

my $s = Stack.new;
my @seen = (^5).map: { $s.push($_) };
is-deeply $s.values, [4, 3, 2, 1, 0], 'cas on the attribute built the whole list';
is $s.elems, 5, '⚛++ on an atomicint attribute counted every push';
is @seen[3][0].value, 2, 'the lexical that held the old head still holds it';
is @seen[0][1], '!head', 'a lexical spelling the name is untouched';

is $s.pop, 4, 'pop takes the head through cas';
is $s.pop, 3, 'and the next one';
is $s.elems, 3, '⚛-- on the attribute';
is-deeply $s.values, [2, 1, 0], 'the rest of the list is intact';
