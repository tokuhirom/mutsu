use Test;

plan 7;

# `$_ ~~ s///` inside `given $x` must write back to the source scalar, because
# the topic aliases the source variable's container.
{
    my $y = "ab";
    given $y { $_ ~~ s/a/X/ }
    is $y, "Xb", 'given $x { $_ ~~ s/// } writes back to the source';
}

# same for `with`
{
    my $z = "hello";
    with $z { $_ ~~ s/l/L/ }
    is $z, "heLlo", 'with $x { $_ ~~ s/// } writes back to the source';
}

# backreferences propagate too
{
    my $s = "11 22 33";
    given $s { $_ ~~ s/\s(\S+)\s(\S+)/-$0-$1/ }
    is $s, "11-22-33", 'given $x { $_ ~~ s/// } with backreferences';
}

# `$_ ~~ tr///` likewise mutates the source
{
    my $t = "abc";
    given $t { $_ ~~ tr/a..c/A..C/ }
    is $t, "ABC", 'given $x { $_ ~~ tr/// } writes back to the source';
}

# a plain assignment to the topic still works (unchanged behaviour)
{
    my $a = 1;
    given $a { $_ = 5 }
    is $a, 5, 'given $x { $_ = ... } still writes back';
}

# a non-destructive match must NOT alter the source
{
    my $m = "hello";
    given $m { $_ ~~ /ell/ }
    is $m, "hello", 'given $x { $_ ~~ /regex/ } leaves the source unchanged';
}

# nested: outer topic restored after inner given
{
    my $outer = "OUT";
    my $inner = "in";
    given $outer {
        given $inner { $_ ~~ s/i/I/ }
        is $_, "OUT", 'outer topic restored after inner given';
    }
}
