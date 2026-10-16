use Test;

# `Supply`'s methods are rows of `Supply`, found by their owner, one routine
# per method whatever the receiver is (ADR-11276 §9.130).

plan 14;

sub s() { Supply.from-list(3, 1, 2, 2, 5) }

# A temporary and a variable receiver reach the same answers.
is-deeply s.map(* * 2).list, (6, 2, 4, 4, 10), 'map on a temporary';
my $s = s;
is-deeply $s.map(* * 2).list, (6, 2, 4, 4, 10), 'map on a variable';
is-deeply s.grep(* > 1).list, (3, 2, 2, 5), 'grep';
is-deeply s.unique.list, (3, 1, 2, 5), 'unique';
is s.rotor(2).list.elems, 2, 'rotor';
is-deeply s.reduce(&[+]).list, (13,), 'reduce';
is-deeply s.max.list, (3, 5), 'max is running';
isa-ok s.delayed(0), Supply, 'delayed(0) answers a Supply';

# The class methods.
is-deeply Supply.merge(s, Supply.from-list(9)).list, (3, 1, 2, 2, 5, 9), 'Supply.merge';
is Supply.zip(s, s).list.elems, 5, 'Supply.zip';

# serialize and sanitize answer the supply, as Rakudo's do for a synchronous
# source.
is-deeply s.serialize.list, (3, 1, 2, 2, 5), 'serialize';
is-deeply s.sanitize.list, (3, 1, 2, 2, 5), 'sanitize';

# tap through a variable and through a temporary.
{
    my @seen;
    $s.tap({ @seen.push($_) });
    s.tap({ @seen.push($_ * 10) });
    is-deeply @seen, [3, 1, 2, 2, 5, 30, 10, 20, 20, 50], 'tap';
}

ok Supply.can('throttle'), '.can finds a row';
