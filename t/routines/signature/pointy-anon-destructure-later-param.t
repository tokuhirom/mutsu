use v6;
use Test;

# A pointy block's parameter may be an anonymous destructure — a bare
# sub-signature with no variable of its own:
#
#     has &.set-content = -> $_, $content, (:$label, :$screen, |) { ... }
#
# (Selkie::UI's tab-bar builder, and Grammar::Editor through it.)
#
# A *first* parameter spelled that way was unpacked by the pointy header itself,
# but a later one went through the per-parameter parser, which had a branch for
# `:(…)`, one for `Type (…)`, and one for `$var (…)` — and none for a bare `(…)`.
# The whole block then failed to parse ("Malformed initializer"). It now delegates
# to the same parameter parser `sub ($c, (:$label))` already uses.

plan 9;

# Positional destructure in second position.
{
    my &f = -> $c, ($a, $b) { "$c:$a:$b" };
    is f(1, (2, 3)), '1:2:3', 'a positional destructure as the second parameter';
}

# Named (hash) destructure in second position, with and without a slurpy.
{
    my &f = -> $c, (:$label) { "$c:$label" };
    is f(1, {:label<x>}), '1:x', 'a named destructure as the second parameter';
}
{
    my &f = -> $_, $content, (:$label, :$screen, |) { "$content:$label:$screen" };
    is f(0, 'c', {:label<a>, :screen<b>, :extra(9)}), 'c:a:b',
        'a named destructure with a slurpy ignores the extra keys';
}

# Third position, and a nested destructure.
{
    my &f = -> $a, $b, ($c, $d) { "$a$b$c$d" };
    is f(1, 2, (3, 4)), '1234', 'a destructure as the third parameter';
}
{
    my &f = -> $a, ($b, ($c, $d)) { "$a$b$c$d" };
    is f(1, (2, (3, 4))), '1234', 'a destructure nested inside a destructure';
}

# The same spelling in a `for` header and in a `sub` signature.
{
    my @out;
    for 1, (2, 3), 4, (5, 6) -> $head, ($a, $b) {
        @out.push: "$head-$a-$b";
    }
    is @out.join(' '), '1-2-3 4-5-6', 'a `for` header takes it too';
    is @out.elems, 2, 'the `for` iterated twice, two items per iteration';
}
{
    my $s = sub ($c, (:$label, |)) { "$c/$label" };
    is $s(1, {:label<y>, :z(2)}), '1/y', 'the `sub` spelling is unchanged';
}

# A first-position destructure still works (it takes the new branch now).
{
    my &f = -> ($a, $b) { $a + $b };
    is f((3, 4)), 7, 'a first-position destructure still unpacks';
}
