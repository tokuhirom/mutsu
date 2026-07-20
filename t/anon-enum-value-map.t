use v6;
use Test;

# The value of an anonymous `enum <...>` expression is a Map in raku, not a
# plain Hash: `.^name`/`.WHAT` report `Map` and `.raku`/`.gist` render as
# `Map.new((...))`. Previously mutsu returned an untagged Hash.
#
# (Tests read the introspection into a separate variable rather than
# `say $e.^name` on the same line as the enum declaration, so this pin does
# not depend on the enum-in-expression `;` parse fix.)

plan 10;

# Word-list anon enum.
{
    my $e = enum <a b c>;
    my $name = $e.^name;
    is $name, 'Map', 'anon enum value .^name is Map';
    my $what = $e.WHAT;
    is $what.^name, 'Map', 'anon enum value .WHAT is the Map type';
}

# The enum members still get their auto-incremented Int values.
{
    my $e = enum <x y z>;
    is x.value, 0, 'anon enum member x == 0';
    is z.value, 2, 'anon enum member z == 2';
}

# Parenthesised anon enum with explicit values.
{
    my $e = enum (a => 5, b => 10);
    my $name = $e.^name;
    is $name, 'Map', 'paren-form anon enum value is a Map';
    is a.value, 5, 'paren-form explicit value preserved';
}

# << >> anon enum.
{
    my $e = enum << p q >>;
    my $name = $e.^name;
    is $name, 'Map', '<< >> anon enum value is a Map';
}

# .raku / .gist render as Map.new((...)).
{
    my $m = (enum <p q>);
    is $m.gist, 'Map.new((p => 0, q => 1))', 'anon enum gist is Map.new(...)';
    ok $m.raku.contains('Map.new'), 'anon enum .raku mentions Map.new';
}

# A Map answers .Map identically (round-trip).
{
    my $e = enum <one two>;
    my $mapped = $e.Map;
    is $mapped.^name, 'Map', 'anon enum value .Map is still a Map';
}
