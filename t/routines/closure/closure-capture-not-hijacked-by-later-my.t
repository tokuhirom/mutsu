use Test;

plan 4;

# A closure created inside a `for` loop captures the loop's PARAMETER. The
# runtime capture path resolved that name by searching the creating frame's
# local slots, which does not know where in the frame a slot was declared -- so
# a same-named `my` appearing LATER in the same compilation unit (a sibling
# block's `my $v`, a different lexical entirely) was found and captured instead.

{
    my @a = 10, 20;
    my @c;
    for @a -> $v is rw { @c.push(-> { $v = $v + 1 }) }
    @c[0]();
    @c[1]();
    is-deeply @a, [11, 21],
        'an escaping closure over an `is rw` for param is not hijacked by a later `my`';
}
{
    my @a = 10, 20;
    my @c;
    for @a -> $v { @c.push(-> { $v }) }
    is-deeply (@c[0](), @c[1]()), (10, 20),
        'a read-only closure over a for param is not hijacked by a later `my`';
}

# The trigger: a sibling block declaring (and using) the same name. Without it
# both blocks above passed, which is why this went unnoticed.
{
    my $v = 1;
    is $v, 1, 'the later same-named `my` still works';
}

# A genuine capture of a `my` declared BEFORE the closure is unaffected: the
# emit-time bake records its slot, so only the "no such local yet" case changes.
{
    my $w = 7;
    my $f = -> { $w };
    for 1, 2 -> $w { }
    is $f(), 7, 'a closure over an earlier same-named `my` still captures it';
}
