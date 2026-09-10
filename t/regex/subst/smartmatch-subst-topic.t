use Test;

plan 9;

# `$_ ~~ s///` must mutate the topic itself in place.
{
    $_ = "11 22";
    $_ ~~ s/\s/-/;
    is $_, "11-22", '$_ ~~ s/// mutates the topic';
}

# bare s/// (implicit topic) still works
{
    $_ = "aa bb";
    s/\s/-/;
    is $_, "aa-bb", 'bare s/// mutates the topic';
}

# backreferences in `$_ ~~ s///`
{
    $_ = "11 22 33";
    $_ ~~ s/\s(\S+)\s(\S+)/-$0-$1/;
    is $_, "11-22-33", '$_ ~~ s/// with backreferences';
}

# inside a for loop, $_ aliases the array element and writes back
{
    my @a = ("11 22", "33 44");
    for @a {
        $_ ~~ s/\s/-/;
    }
    is @a.join(' '), '11-22 33-44', 'for { $_ ~~ s/// } writes back to array';
}

# for loop with backreferences (roast S04-statements/for.t test 97)
{
    my @strs = ("11 22 33", "44 55 66", "77 88 99");
    for @strs {
        $_ ~~ s/\s(\S+)\s(\S+)/-$0-$1/;
    }
    is @strs.join(' '), '11-22-33 44-55-66 77-88-99',
        'substitution with backreferences in for loop';
}

# a normal variable on the LHS still writes back
{
    my $x = "11 22";
    $x ~~ s/\s/-/;
    is $x, "11-22", '$x ~~ s/// mutates a named variable';
}

# `$_ ~~ tr///` mutates the topic in place
{
    $_ = "abc";
    $_ ~~ tr/a..c/A..C/;
    is $_, "ABC", '$_ ~~ tr/// mutates the topic';
}

# `$_ ~~ /regex/` does NOT mutate the topic (non-destructive)
{
    $_ = "hello";
    my $m = ($_ ~~ /ell/);
    is $_, "hello", '$_ ~~ /regex/ leaves the topic unchanged';
    ok $m, '$_ ~~ /regex/ still matches';
}
