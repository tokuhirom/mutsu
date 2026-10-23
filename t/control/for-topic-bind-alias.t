use Test;

plan 6;

# A scalar bound to the implicit topic of `for @a` keeps the element's own
# container, so later writes to the element are visible through it (#14758).
{
    my @e = 1, 2, 3;
    my $bound;
    for @e { $bound := $_ if $_ == 2 }
    @e[1] = 20;
    is $bound, 20, 'bind of the topic under a statement modifier aliases the element';
}

{
    my @f = 1, 2, 3;
    my $b;
    for @f { $b := $_; last }
    @f[0] = 7;
    is $b, 7, 'a read through the bound scalar sees a write to the element';
    $b = 42;
    is @f[0], 42, 'a write through the bound scalar reaches the element';
}

{
    my @g = 1, 2, 3;
    my $b;
    for @g { if $_ == 3 { $b := $_ } }
    @g[2] = 30;
    is $b, 30, 'bind inside an if block aliases the element';
}

# Only a bind takes the element's container; passing `$_` on stays a value.
{
    my @list = 1, 2, 2, 3;
    my @uniq = gather for @list {
        state $previous = take $_;
        next if $_ === $previous;
        $previous = take $_;
    }
    is @uniq.join(" "), "1 2 3", 'take $_ hands out the value, not the element container';
}

{
    my @e = 1, 2, 3;
    my $z;
    for @e { my $y := $_; $z = $y if $_ == 2 }
    @e[1] = 20;
    is $z, 2, 'a plain copy of a bound topic is still a copy';
}

