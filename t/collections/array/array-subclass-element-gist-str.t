use Test;

# An `is Array` / `is List` subclass instance assigned into an array arrives
# ITEMIZED (`my @h = $c` compiles an itemization of `$c`), so the instance that
# needs method dispatch sits one Scalar down from the element slot. The probes
# that decide "does rendering this list need the interpreter?" only looked
# through a `:=`-bound cell, not through that itemization, so the pure renderers
# answered for the element and printed the `SA()` type-object-shaped fallback:
# `@h.gist`, `@h.Str` and `@h.join` all came back as `SA()`, while `say @h` (the
# implicit gist, which takes a different route) was right all along.
#
# Every expectation here was measured against rakudo, and the file passes under
# `raku` unchanged.

plan 18;

class SA is Array { }
class SL is List { }
class P { has $.x }

# --- the reported shapes: an `is Array` element ------------------------------
{
    my $c = SA.new(3, 2, 1, 4);
    my @h = $c;

    is @h.elems, 1, 'an Array subclass instance is stored as one element';
    is @h.gist, '[[3 2 1 4]]', 'explicit .gist renders the element through its own gist';
    is ~@h, '3 2 1 4', 'the implicit gist (say/~) was always right and stays right';
    is @h.Str, '3 2 1 4', 'explicit .Str stringifies the element through its own Str';
    is @h.join('|'), '3 2 1 4', '.join stringifies it the same way';
    is @h.raku, '[[3, 2, 1, 4],]', '.raku is unchanged';
}

# --- the same shapes for an `is List` subclass -------------------------------
{
    my $l = SL.new(3, 2, 1);
    my @g = $l;

    is @g.gist, '[(3 2 1)]', 'a List subclass element keeps its parenthesized gist';
    is @g.Str, '3 2 1', 'and stringifies through its own Str';
    is @g.join('|'), '3 2 1', 'and joins through it too';
}

# --- nested: the array holding the instance is itself an element -------------
{
    my $c = SA.new(3, 2, 1, 4);
    my @h = $c;
    my @outer = @h,;

    is @outer.gist, '[[[3 2 1 4]]]', 'the nested case renders at both levels';
    is @outer.Str, '3 2 1 4', 'and stringifies through the leaf instance';
}

# --- controls that were already correct and must stay so ---------------------
{
    my $p = P.new(x => 1);
    my @q = $p;
    is @q.gist, '[P.new(x => 1)]', 'a NON-container instance element is unchanged';
}

{
    my @r = [1, 2],;
    is @r.gist, '[[1 2]]', 'a plain array element is unchanged';
    is @r.Str, '1 2', 'and so is its Str';
}

{
    my $c = SA.new(1, 2);
    is $c.gist, '[1 2]', "the instance's OWN gist is unchanged";
    is $c.Str, '1 2', "and its OWN Str";
}

# An element pushed (rather than assigned) is NOT itemized -- that spelling
# always worked, and is the control proving the fix did not move the answer.
{
    my $c = SA.new(1, 2);
    my @h;
    @h.push($c);
    is @h.gist, '[[1 2]]', 'a pushed (non-itemized) element still gists correctly';
    is @h.Str, '1 2', 'and still stringifies correctly';
}
