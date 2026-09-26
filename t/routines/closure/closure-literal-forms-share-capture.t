use Test;

# #9454: the closure opcodes (`MakeAnonSub` for `{ ... }`, `MakeAnonSubParams`
# for `sub { ... }` and placeholder blocks, `MakeLambda` for `-> { ... }`)
# used to carry four copies of the capture pipeline, and the copies ran
# different steps. They now share one builder, so each scenario below must give
# the same answer through every literal form.

plan 6;

# A read-only `:=` alias the loop re-binds per iteration: each closure keeps
# its own iteration's binding.
{
    my @a = 1, 2, 3;
    my (@b, @l, @s, @p);
    loop (my $i = 0; $i < 3; $i++) {
        my $in := @a[$i];
        @b.push: { $in * 10 };
        @l.push: -> { $in * 10 };
        @s.push: sub { $in * 10 };
        @p.push: { $^x + $in };
    }
    is-deeply @b».(), [10, 20, 30], 'bare block keeps its iteration binding';
    is-deeply @l».(), @b».(), 'pointy block agrees';
    is-deeply @s».(), @b».(), 'anon sub agrees';
    is-deeply @p».(0), [1, 2, 3], 'placeholder block agrees';
}

# A closure created inside a routine with a declared return type does not
# inherit that type: only the routine's own return is checked.
{
    sub typed(--> Int) {
        my @r = ({ "b" })(), (-> { "l" })(), (sub { "s" })(), ({ $^x })("p");
        @r.join.chars;
    }
    is typed(), 4, 'no closure form enforces the enclosing routine return type';
}

# A declared `-->` on the closure itself is still enforced.
{
    my &f = -> --> Int { "str" };
    throws-like { f() }, X::TypeCheck::Return, 'a closure literal keeps its own return type';
}
