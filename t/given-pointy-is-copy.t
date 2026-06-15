use Test;

plan 7;

# `given LITERAL -> $_ is copy` makes $_ a fresh writable copy.
given "a 2 3" -> $_ is copy {
    $_ = "x";
    is $_, "x", 'given literal -> $_ is copy allows $_ = ...';
}

# `is copy` over a substitution-assignment (the subst.t case).
given 'a 2 3' -> $_ is copy {
    s[\d] += 5;
    is $_, 'a 7 3', 's[...] += 5 inside given -> $_ is copy';
}

# `is copy` over a variable topic must NOT write back to the source.
{
    my $x = "orig";
    given $x -> $_ is copy {
        $_ = "changed";
    }
    is $x, "orig", 'given $x -> $_ is copy does not write back';
}

# Plain `given $x { ... }` (no pointy) still topicalizes writably.
{
    my $y = "orig";
    given $y {
        $_ = "changed";
    }
    is $y, "changed", 'given $x { $_ = ... } still works';
}

# Aliasing pointy array param still writes back through .push.
{
    my @a = 1, 2;
    given @a -> @p {
        @p.push(3);
    }
    is-deeply @a, [1, 2, 3], 'given @a -> @p { @p.push } writes back';
}

# `is copy` array param is a detached copy.
{
    my @a = 1, 2;
    given @a -> @p is copy {
        @p.push(3);
    }
    is-deeply @a, [1, 2], 'given @a -> @p is copy does not write back';
}

# Named scalar `is copy` param still works.
{
    my $x = 5;
    given $x -> $n is copy {
        $n = 99;
    }
    is $x, 5, 'given $x -> $n is copy leaves source untouched';
}
