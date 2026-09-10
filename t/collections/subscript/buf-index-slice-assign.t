use Test;

# `$b[i] = v` and `$b[i,j] = v,w` on a Buf were both broken: the generic
# index-assign fallback treated the Buf `Instance` as an uninitialized
# container and silently replaced it with a plain Array/Hash instead of
# mutating its underlying byte storage (roast S09-subscript/slice.t test 31,
# "can assign to a Buf slice").

plan 10;

{
    my $b = Buf.new(0, 0, 0);
    $b[0] = 5;
    is-deeply $b, Buf.new(5, 0, 0), 'single-index assignment mutates the Buf';
}

{
    my $b = Buf.new(0, 0);
    $b[0, 1] = 2, 3;
    is-deeply $b, Buf.new(2, 3), 'can assign to a Buf slice';
}

{
    # byte-masking: values are wrapped mod 256, like a native uint8 array.
    my $b = Buf.new(0, 0);
    is ($b[0] = 300), 300, 'single-index assignment expression yields the original value';
    is-deeply $b, Buf.new(44, 0), 'single-index assignment masks the stored byte to 0..255';
}

{
    my $b = Buf.new(0, 0);
    is-deeply ($b[0, 1] = 300, -1), (44, 255), 'slice assignment expression yields masked bytes';
    is-deeply $b, Buf.new(44, 255), 'slice assignment masks each stored byte to 0..255';
}

{
    # out-of-bounds indices autoextend with zero fill, like a native array.
    my $b = Buf.new(0, 0);
    $b[4] = 9;
    is-deeply $b, Buf.new(0, 0, 0, 0, 9), 'single-index assignment autoextends with zero fill';
}

{
    my $b = Buf.new(0, 0);
    $b[3, 4] = 7, 8;
    is-deeply $b, Buf.new(0, 0, 0, 7, 8), 'slice assignment autoextends with zero fill';
}

{
    my $b = Blob.new(1, 2, 3);
    dies-ok { $b[0] = 9 }, 'single-index assignment to an immutable Blob dies';
    dies-ok { $b[0, 1] = 9, 8 }, 'slice assignment to an immutable Blob dies';
}
