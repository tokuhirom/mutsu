use Test;

plan 7;

# A `given` STATEMENT MODIFIER introduces no block, so a placeholder in the
# statement it modifies belongs to the ENCLOSING routine and is one of its
# parameters. mutsu treated the modified statement like a `given {}` block body:
# its placeholders were not collected into the routine's signature, and were
# instead bound to the topic. Digest::SHA3's rotate helper is exactly this
# shape and came out as a one-parameter routine:
#
#     sub ROL64 { ($^a +> (64 - $_) +| $a +< $_) % (1 +< 64) given $^n % 64 }

{
    sub r1 { "a=$^a n=$^n" }
    is r1(10, 20), "a=10 n=20", 'control: two placeholders, no modifier';

    sub r2 { "a=$^a n=$^n topic=$_" given $^n % 64 }
    is r2(10, 20), "a=10 n=20 topic=20",
        'a `given` modifier does not steal the body placeholders';

    sub r3 { "a=$^a topic=$_" given $^n }
    is r3(10, 20), "a=10 topic=20",
        '...and the topic is the modifier expression, not the first argument';

    sub r4 { my $t = $^n % 64; "a=$^a n=$^n t=$t" }
    is r4(10, 20), "a=10 n=20 t=20",
        'control: the same computation without the modifier';
}

# The real thing: a 64-bit rotate.
{
    sub ROL64 { ($^a +> (64 - $_) +| $a +< $_) % (1 +< 64) given $^n % 64 }
    is ROL64(1, 1), 2, 'ROL64 rotates by one';
    is ROL64(0x0102030405060708, 8), 144964032628459521, 'ROL64 rotates by eight';
}

# A `given` BLOCK is unchanged: its body IS its own scope, and a placeholder
# there is the block's parameter, bound to the topic.
{
    my $out = do given 42 { $^a };
    is $out, 42, 'a placeholder in a given BLOCK still binds the topic';
}
