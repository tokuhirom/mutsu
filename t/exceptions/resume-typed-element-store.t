use Test;

# `.resume` of a refused typed element store continues in place and leaves the
# type object in the slot, as Rakudo does (#14153). Expected values are the
# `raku` output.

plan 6;

my @log;
{
    my Int @b; @b[0] = "x"; @log.push: @b.raku;
    my Int %i; %i<a> = "z"; @log.push: %i.raku;
    my Int @c = 1, "y"; @log.push: @c.raku;
    CATCH { default { @log.push: "c"; .resume } }
}
is-deeply @log.List, ("c", "Array[Int].new(Int)", "c", "(my Int % = :a(Int))", "c",
    "Array[Int].new(1, Int)"), 'element stores and a list declaration resume';

my @n;
{
    my Int @d = "a", 2, "b";
    @n.push: @d.raku;
    CATCH { default { @n.push: "c"; .resume } }
}
is-deeply @n.List, ("c", "c", "Array[Int].new(Int, 2, Int)"), 'every refused element is offered';

my @m;
{
    my Int @e;
    my $r = (@e[1] = "q");
    @m.push: @e.raku;
    CATCH { default { .resume } }
}
is @m[0], 'Array[Int].new(Int, Int)', 'a gap before the resumed slot is vivified';

my @o;
{
    my Int @f;
    try { @f[0] = "x"; CATCH { default { @o.push: "caught" } } }
    @o.push: @f.elems;
}
is-deeply @o.List, ("caught", 0), 'a handler that does not resume leaves the array alone';

my Int @g;
throws-like { @g[0] = "x" }, X::TypeCheck::Assignment, 'an unhandled refusal still throws';

my Int @h = 1, 2;
{
    @h[1] = "z";
    CATCH { default { .resume } }
}
is @h.raku, 'Array[Int].new(1, 2)', 'a refused assignment keeps an existing slot';
