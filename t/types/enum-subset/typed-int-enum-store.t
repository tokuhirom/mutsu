use Test;

# A value of an Int-based enum stored in, bound to, or passed as a native int,
# and the in-place step of a `uint` lexical (#17337).

plan 14;

{
    my int $c = 1 cmp 2;
    is $c, -1, 'my int takes the Order cmp returns';
    is $c.WHAT, Int, 'as its Int value';
    my int8 $m = More;
    is $m, 1, 'a sized native int takes an enum value';
    my uint $u = Less;
    is $u, 18446744073709551615, 'a uint wraps a negative enum value';
}

{
    sub f(int $x) { $x * 2 }
    is f(More), 2, 'an int parameter binds an enum value';
    my int $acc = 0;
    $acc = $acc + ($_ cmp 5) for ^10;
    is $acc, -5 + 4, 'cmp results summed through an int lexical';
}

{
    my uint $i = 1;
    --$i;
    is $i, 0, 'a uint steps down to zero';
    --$i;
    is $i, 18446744073709551615, 'and wraps below it';
    ++$i;
    is $i, 0, 'and wraps back up';
    my uint8 $b = 255;
    ++$b;
    is $b, 0, 'a uint8 wraps at its width';
}

{
    my uint $j = 0;
    $j++ for ^1000;
    is $j, 1000, 'a uint counter in a loop';
    my uint $k = 5;
    my $old = $k++;
    is $old, 5, 'postfix ++ on a uint answers the old value';
    is $k, 6, 'and stores the new one';
    my uint $z = 0;
    my $before = $z--;
    is "$before $z", '0 18446744073709551615', 'postfix -- below zero wraps';
}
