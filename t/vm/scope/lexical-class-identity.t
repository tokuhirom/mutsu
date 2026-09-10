use v6;
use Test;

plan 8;

# Two `my class Foo` declared in separate `gather` blocks are distinct types;
# instances produced by the first must keep the first class's method bodies even
# after the second same-named class is declared (the gather bodies' instances
# escape into @a / @b, so both classes are simultaneously live).
{
    my @a = gather { my class Foo { method val { 1 } }; take Foo.new };
    my @b = gather { my class Foo { method val { 99 } }; take Foo.new };
    is @a[0].val, 1, "first lexical class keeps its own method body";
    is @b[0].val, 99, "second lexical class has its own method body";
}

# Mixed values from two same-named lexical classes coexist correctly.
{
    my @vals = gather {
        my class N { method v { 3 } }
        take N.new;
    };
    my @vals2 = gather {
        my class N { method v { 42 } }
        take N.new;
    };
    is @vals[0].v,  3,  "first gather's N keeps v=3 after second N declared";
    is @vals2[0].v, 42, "second gather's N has v=42";
}

# A single declaration inside a loop is ONE type across iterations.
{
    my @t;
    for 1..3 { my class L { }; @t.push: L }
    ok @t[0] === @t[1], "loop-declared lexical class is the same type each iteration";
    ok @t[1] === @t[2], "loop-declared lexical class identity is stable";
}

# The user-facing name is unaffected by the internal mangling.
{
    my @a = gather { my class Named { }; take Named.new };
    my @b = gather { my class Named { }; take Named.new };
    is @a[0].^name, "Named", "first same-named lexical class reports its bare name";
    is @b[0].^name, "Named", "second same-named lexical class reports its bare name";
}
