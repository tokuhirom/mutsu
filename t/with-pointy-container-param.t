use Test;

# `with EXPR -> @m { }` desugars to a temp holding the once-evaluated condition
# plus `my @m = <temp>`. The compiler's "assigning a `$` scalar variable to an
# `@` target itemizes it" rule then fired on that temp, so the block's `@m` saw
# ONE element -- the whole list -- instead of the list. A desugaring temp is not
# a Scalar container the source wrote, so it must not itemize.
#
# The lvalue spelling (`with $r -> @m`) was already right: it routes through
# `given`'s alias bind rather than through the assignment path.

plan 8;

{
    my $r = ("a<!--x-->b<!--yy-->c" ~~ m:g/('<!--') \s* (\w+) \s* ('-->')/);
    is $r.elems, 2, 'the :g match itself has two results';

    with ("a<!--x-->b<!--yy-->c" ~~ m:g/('<!--') \s* (\w+) \s* ('-->')/) -> @m {
        is @m.elems, 2, 'an expression topic binds the list, not a wrapper';
    }

    with $r -> @m {
        is @m.elems, 2, '... and so does an lvalue topic (already worked)';
    }
}

with (1, 2, 3) -> @m {
    is @m.elems, 3, 'a literal list topic binds all three elements';
}

with (1, 2, 3) -> $m {
    is $m.elems, 3, '... and a `$` parameter still sees the whole list';
}

with %(:a(1), :b(2)) -> %h {
    is %h.elems, 2, 'a `%` parameter binds the hash, not a wrapper';
}

# Controls: the itemizing rule this fix narrows must keep applying to a real
# `$`-scalar container.
{
    my $x = (1, 2, 3);
    my @a = $x;
    is @a.elems, 1, '`my @a = $x` still itemizes the scalar container';
}

{
    my @src = 1, 2, 3;
    my @b = @src;
    is @b.elems, 3, '... while `my @b = @src` still copies the elements';
}
