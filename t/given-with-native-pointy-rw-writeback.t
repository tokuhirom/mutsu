use Test;

plan 6;

{
    my int $x = 1;
    given $x -> int $v is rw { $v = 99 }
    is $x, 99, 'given native int pointy param is rw writes back';
}

{
    my int $x = 1;
    with $x -> int $v is rw { $v = 99 }
    is $x, 99, 'with native int pointy param is rw writes back';
}

{
    my int $x = 1;
    given $x -> int $v is rw { }
    is $x, 1, 'given native int pointy param is rw, unchanged, no writeback';
}

{
    my str $x = 'a';
    given $x -> str $v is rw { $v = 'z' }
    is $x, 'z', 'given native str pointy param is rw writes back';
}

{
    my int $x = 5;
    my int $sum = 0;
    given $x -> int $v is rw {
        $v = $v + 1;
        $sum = $v;
    }
    is $x, 6, 'given native int pointy rw writeback reflects in-body mutation chain';
    is $sum, 6, 'in-body read of the rw pointy param sees its own mutation';
}
