use Test;

plan 6;

{
    sub f(Buf $d) { $d[0] = 3 }
    my $b = Buf.new(0x80, 1);
    f($b);
    is $b.raku, 'Buf.new(3,1)', 'non-rw typed Buf param element assign (control)';
}

{
    sub f($d is rw) { $d[0] = 3 }
    my $b = Buf.new(0x80, 1);
    f($b);
    is $b.raku, 'Buf.new(3,1)',
        'untyped rw param element assign mutates the caller Buf in place';
}

{
    sub f(Buf $d is rw) { $d[0] = 3 }
    my $b = Buf.new(0x80, 1);
    f($b);
    is $b.raku, 'Buf.new(3,1)',
        'typed rw Buf param element assign (the Cro FrameParser shape)';
}

{
    sub f($d is rw) { $d[1] = 9 }
    my $b = Buf.new(0x80, 1);
    f($b);
    is $b.raku, 'Buf.new(128,9)',
        'rw param element assign at a non-zero index does not autoviv an Array';
}

{
    sub f(Array $d is rw) { $d[0] = 3 }
    my $arr = [9, 1];
    f($arr);
    is $arr.raku, '$[3, 1]', 'rw Array param element assign still works (control)';
}

{
    sub f($d is rw) { $d[0] +&= 0x7F }
    my $b = Buf.new(0x80, 1);
    f($b);
    is $b.raku, 'Buf.new(0,1)',
        'compound assignment (+&=) through an rw Buf param element';
}

done-testing;
