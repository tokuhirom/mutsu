use v6;
use Test;

# The value of a NAMED `enum Foo <...>` expression is a Map in raku, just like
# the anonymous form: `.^name`/`.WHAT` report `Map` and `.raku` renders as
# `$(Map.new((...)))`. Previously mutsu returned Any for the named form (the
# expression-position result was never pushed for a named enum). See
# t/anon-enum-value-map.t for the anonymous companion.

plan 13;

# Word-list named enum used in expression position.
{
    my $e = enum Foo <a b c>;
    is $e.^name, 'Map', 'named enum value .^name is Map';
    is $e.WHAT.^name, 'Map', 'named enum value .WHAT is the Map type';
    is $e.raku, '$(Map.new((:a(0),:b(1),:c(2))))', 'named enum value .raku matches raku';
}

# The named type and its members still install correctly.
{
    my $e = enum Bar <x y z>;
    is Bar.^name, 'Bar', 'named enum type name installed';
    is x.^name, 'Bar', 'enum member reports the enum type as its name';
    is x.value, 0, 'named enum member x == 0';
    is z.value, 2, 'named enum member z == 2';
}

# Parenthesised named enum with explicit values.
{
    my $e = enum Nums (one => 1, two => 2);
    is $e.^name, 'Map', 'paren-form named enum value is a Map';
    is $e.raku, '$(Map.new((:one(1),:two(2))))', 'paren-form named enum value .raku';
    is one.value, 1, 'paren-form explicit value preserved';
    is Nums.^name, 'Nums', 'paren-form named type installed';
}

# A named enum as a bare statement must not corrupt the stack (the pushed Map
# is a harmless sink).
{
    sub f() { enum Colours <red green blue>; 42 }
    is f(), 42, 'named enum as a non-tail statement does not corrupt the return value';
}

# A named enum declared as a bare file-scope statement still installs its members.
{
    enum Planets <mercury venus earth>;
    is earth.value, 2, 'the statement-form named enum still installed its members';
}

# Note: `my $e = do { enum Foo <...> }` (an EXPLICIT do-block wrapper) still yields
# Any, not the Map — a separate pre-existing gap in the scope-isolating DoBlock
# value path that affects the anonymous form too, out of scope for this fix.
