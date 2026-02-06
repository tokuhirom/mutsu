use Test;
plan 8;
# basic loop
{
    my $i = 0;
    loop ($i = 0; $i < 10; $i++) {}
    is($i, 10, 'basic loop');
}
# loop with last
{
    my $i = 0;
    loop ($i = 0; $i < 10; $i++) {
        if $i == 5 {
            last;
        }
    }
    is($i, 5, 'loop with last');
}
# infinite loop with last
{
    my $i = 0;
    loop (;;) { $i++; last; }
    is($i, 1, 'infinite loop with last');
}
# bare loop
{
    my $loopvar = 0;
    loop {
        last if ++$loopvar == 3;
    }
    is($loopvar, 3, 'bare loop exited after 3 iterations');
}
# loop with variable declaration in init
{
    my $count = 0;
    loop (my $j = 0; $j < 10; $j++) { $count++; };
    is($count, 10, 'loop with variable declaration in init');
}
# next performs continue expression
{
    my $i;
    my $continued;
    loop ($i = 0;; $continued = 1) {
        last if $i;
        $i++;
        next;
    }
    ok($continued, 'next performs continue expression');
}
# until loop
{
    my $i = 0;
    until $i >= 5 { $i++; };
    is($i, 5, 'until loop works');
}
# unless
{
    my $x = 0;
    unless $x { $x = 42; }
    is($x, 42, 'unless works');
}
