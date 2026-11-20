use Test;

# `++`/`--` of a `my int` local take a narrow step when the compiler has
# proved the slot is a plain native int of this frame (ADR-18357 stage 1).
# Every shape keeps its answer, including the ones that fall back to the
# generic step.

plan 18;

sub loop-count(int $n) { my int $p = 0; $p++ while $p < $n; $p }
is loop-count(1000), 1000, 'postfix ++ in a statement-modifier loop';

{
    my int $p = 5;
    is $p++, 5, 'postfix ++ answers the old value';
    is $p, 6, '... and steps the variable';
    is ++$p, 7, 'prefix ++ answers the new value';
    is $p--, 7, 'postfix -- answers the old value';
    is --$p, 5, 'prefix -- answers the new value';
}

{
    my int $p = 9223372036854775807;
    $p++;
    is $p, -9223372036854775808, '++ wraps at the top of int';
    $p--;
    is $p, 9223372036854775807, '-- wraps at the bottom of int';
}

{
    my int $p = 0;
    my &peek = { $p };
    $p++;
    $p++;
    is peek(), 2, 'a closure reading the variable sees every step';
}

{
    my int $p = 0;
    my &bump = { $p++ };
    bump() for ^3;
    $p++;
    is $p, 4, 'steps from a closure and from the frame share one variable';
}

sub counter() { state Int $n = 0; ++$n }
counter() for ^2;
is counter(), 3, 'a state Int keeps its value across calls';

{
    my int $total = 0;
    my int $i = 0;
    while $i < 10 { $total += $i; $i++ }
    is $total, 45, 'a while loop over a stepped int';
}

{
    my int $p = 3;
    my $q = $p++ + $p++;
    is $q, 7, 'two steps in one expression';
    is $p, 5, '... leave the variable twice stepped';
}

{
    my Int $boxed = 1;
    $boxed++;
    is $boxed, 2, 'a boxed Int local still steps';
    my $untyped = 'a9';
    $untyped++;
    is $untyped, 'b0', 'an untyped local still takes the string step';
}

sub param-step(int $n is copy) { $n++; $n }
is param-step(41), 42, 'an `is copy` int parameter steps';

{
    my int $p = 0;
    $p++ for ^100;
    is $p, 100, 'steps in a for loop';
}
