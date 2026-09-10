use v6;
use Test;

# A parenthesized pointy-block signature `-> ($a, $b)` is ONE parameter that
# binds a single list/Capture argument and unpacks it (destructuring), NOT two
# separate parameters (`-> $a, $b`). This distinction matters for `.map`:
#   .map(-> ($k, $v) {...})  destructures each list element
#   .map(-> $a, $b {...})    chunks the list two-at-a-time

plan 9;

# .map with a destructuring block: each element is a 2-list, unpacked.
{
    my @pairs = (1, 'a'), (2, 'b'), (3, 'c');
    my @out = @pairs.map(-> ($n, $v) { "$n=$v" });
    is @out.join(','), '1=a,2=b,3=c', '.map(-> ($a,$b)) destructures each element';
}

# .map with two plain params chunks two-at-a-time (unchanged behavior).
{
    my @out = (1, 2, 3, 4).map(-> $a, $b { "$a$b" });
    is @out.join(','), '12,34', '.map(-> $a, $b) chunks two-at-a-time';
}

# The real Humming-Bird cookie-decode shape: split then destructuring map.
{
    my @r = "sid=abc123".split(/\s/, 2, :skip-empty)
                        .map(*.split('=', 2, :skip-empty))
                        .map(-> ($name, $value) { "$name|$value" })
                        .flat;
    is @r.join(','), 'sid|abc123', 'split + destructuring map (cookie-decode shape)';
}

# for over a list-of-lists with a destructuring pointy param.
{
    my @log;
    for ((1, 2), (3, 4)) -> ($k, $v) { @log.push("$k:$v") }
    is @log.join(','), '1:2,3:4', 'for over list-of-lists destructures -> ($k,$v)';
}

# for two-at-a-time (no parens) keeps chunking semantics.
{
    my @log;
    for (1, 2, 3, 4) -> $a, $b { @log.push("$a-$b") }
    is @log.join(','), '1-2,3-4', 'for -> $a, $b chunks two-at-a-time';
}

# A destructuring block bound directly, called with one list argument.
{
    my $b = -> ($a, $c) { "$a/$c" };
    is $b((10, 20)), '10/20', 'pointy destructuring block called with one list arg';
}

# A single-element paren stays a lone parameter (no destructuring change).
{
    my $b = -> ($x) { $x * 2 };
    is $b(21), 42, '-> ($x) is a single ordinary parameter';
}

# Nested data: list of 3-tuples.
{
    my @rows = ('a', 1, True), ('b', 2, False);
    my @out = @rows.map(-> ($name, $num, $flag) { "$name$num$flag" });
    is @out.join(','), 'a1True,b2False', 'destructuring map with 3 elements';
}

# rw pointy destructuring still parses and binds.
{
    my @pairs = (1, 2), (3, 4);
    my @sums = @pairs.map(-> ($a, $b) { $a + $b });
    is @sums.join(','), '3,7', 'destructuring map computing over elements';
}
