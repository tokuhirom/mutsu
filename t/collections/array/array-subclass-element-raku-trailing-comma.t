use Test;

# A real array with a single Iterable element renders a trailing comma, so the
# `.raku` round-trip does not flatten it. An `is Array` / `is Hash` subclass
# instance does Iterable too, but it reaches `.raku` through method dispatch:
# the leaf is rendered by the interpreter and spliced back in as a placeholder,
# and the placeholder used to erase the fact that the leaf was Iterable.

plan 9;

class SA is Array { }
class SH is Hash { }
class P { has $.x }

# The instance has to arrive through a scalar: `my @h = SA.new(...)` flattens
# its elements, exactly as `my @h = [1, 2]` would.
{
    my $c = SA.new(3, 2, 1, 4);
    my @h = $c;
    is @h.elems, 1, 'an Array subclass instance is stored as one element';
    is @h.raku, '[[3, 2, 1, 4],]', 'and a lone one renders with the trailing comma';
}

{
    my $h = SH.new;
    $h<k> = 1;
    my @c = $h;
    is @c.raku, '[{:k(1)},]', 'a lone Hash subclass instance renders with it too';
}

# A non-Iterable instance must NOT gain one.
{
    my $p = P.new(x => 1);
    my @d = $p;
    is @d.raku, '[P.new(x => 1)]', 'a plain instance element takes no trailing comma';
}

# Arity is still part of the rule: two elements never take one.
{
    my @b = [SA.new(1, 2), 3];
    is @b.raku, '[[1, 2], 3]', 'a two-element array takes no trailing comma';
}

{
    my ($s1, $s2) = SA.new(1), SA.new(2);
    my @two = $s1, $s2;
    is @two.raku, '[[1], [2]]', 'two subclass instances take no trailing comma either';
}

# The controls that were already correct, so the fix cannot regress them.
{
    is [[1, 2],].raku, '[[1, 2],]', 'a lone plain array element still renders the comma';
}

{
    my $p = [1, 2];
    my @q = $p;
    is @q.raku, '[[1, 2],]', 'a lone array through a scalar still renders the comma';
}

{
    my @e = 1;
    is @e.raku, '[1]', 'a lone non-Iterable element takes no trailing comma';
}
