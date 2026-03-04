use Test;
plan 4;

# Unicode copyright symbol as infix operator
{
    sub infix:<©>($a, $b) { $a + $b }
    is(1 © 2, 3, 'Unicode copyright symbol as infix operator');
}

# Word-like operator via double-angle-bracket delimiter
{
    my sub infix:<<bg>>($a, $b) { $a + $b }
    is(1 bg 2, 3, 'double-angle-bracket delimited infix operator');
}

# Unicode multiplication sign as infix operator
{
    sub infix:<×>($a, $b) { $a * $b }
    is(5 × 3, 15, 'Unicode multiplication sign as infix operator');
}

# Guillemet delimiter form with word operator
{
    my sub infix:«myop»($a, $b) { $a - $b }
    is(10 myop 3, 7, 'guillemet delimited infix operator');
}
