use Test;

# `for @a` over a plain Array reads the array in place, one element per
# iteration (#9158): nothing is copied at entry, and the body sees an element
# it pushes or stores before the loop gets there, as Rakudo's Array iterator
# does.

plan 11;

{
    my @a = 1, 2, 3;
    my @seen;
    for @a -> $v { @a[2] = 9 if $v == 1; @seen.push($v) }
    is @seen, [1, 2, 9], 'a later element stored by the body is seen';
}
{
    my @a = 1, 2, 3;
    my @seen;
    for @a { @a.push(4) if $_ == 1; @seen.push($_) }
    is @seen, [1, 2, 3, 4], 'an element pushed by the body is iterated';
}
{
    my @a = 1, 2, 3;
    my @seen;
    for @a { @a.shift if $_ == 1; @seen.push($_) }
    is @seen, [1, 3], 'a shift by the body moves the rest down, as in Rakudo';
}
{
    my @a = 1, 2, 3;
    for @a { $_ *= 2 }
    is @a, [2, 4, 6], 'the topic still aliases the element';
    for @a -> $x is rw { $x += 1 }
    is @a, [3, 5, 7], 'an rw parameter still aliases the element';
}
{
    my @a = ^5;
    my @seen;
    for @a { last if $_ == 2; @seen.push($_) }
    is @seen, [0, 1], 'last';
    my @r = do for @a { $_ * 10 };
    is @r, [0, 10, 20, 30, 40], 'a collecting loop';
}
{
    my @a = [1, 2], [3, 4];
    my @n;
    for @a -> @p { @n.push(@p.elems) }
    is @n, [2, 2], 'an @-parameter binds each element';
    my $r = '';
    for <a b>.kv -> $i, $v { $r ~= "$i$v " }
    is $r, '0a 1b ', 'a multi-parameter loop still chunks';
}

# A `last` early in a loop over a big array does not depend on its size.
{
    my @big = ^200_000;
    my $n = 0;
    for ^2000 { for @big { $n++; last } }
    is $n, 2000, 'for @big { last } runs one iteration per pass';
    my @empty;
    my $ran = False;
    for @empty { $ran = True }
    nok $ran, 'an empty array runs no iteration';
}
