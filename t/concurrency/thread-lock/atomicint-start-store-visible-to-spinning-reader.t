use Test;

# A `start` block's atomic store on a captured `atomicint` is seen by a main
# thread that is already reading the variable in a loop (the "spin until
# another thread sets it" shape of the atomicint documentation). The reads
# must keep the binding's one shared cell instead of boxing a fresh one each
# time.

plan 6;

{
    my atomicint $a = 0;
    start { $a ⚛= 1 };
    my $n = 0;
    while $a == 0 { sleep 0.01; last if ++$n > 300 }
    is $a, 1, '$a ⚛= 1 in a start block ends a spinning while loop';
}
{
    my atomicint $a = 0;
    start { atomic-assign($a, 1) };
    my $n = 0;
    while $a == 0 { sleep 0.01; last if ++$n > 300 }
    is $a, 1, 'atomic-assign in a start block';
}
{
    my atomicint $b = 0;
    start { $b ⚛= 1 };
    my $m = 0;
    loop { last if $b != 0; sleep 0.01; last if ++$m > 300 }
    is $b, 1, 'a bare loop with last if';
}
{
    my atomicint $c = 0;
    my $p = start { $c⚛++ for ^100 };
    my $k = 0;
    while $c < 100 { sleep 0.01; last if ++$k > 1000 }
    await $p;
    is $c, 100, 'increments from a start block are all seen';
}
{
    my atomicint $d = 5;
    start { $d ⚛-= 2 };
    my $j = 0;
    while $d == 5 { sleep 0.01; last if ++$j > 300 }
    is $d, 3, 'a compound atomic assignment';
}
{
    my atomicint $e = 0;
    my $first = $e;
    start { $e ⚛= 9 };
    my $i = 0;
    while $e == 0 { sleep 0.01; last if ++$i > 300 }
    is $e, 9, 'a read before the start block does not detach the cell';
}
