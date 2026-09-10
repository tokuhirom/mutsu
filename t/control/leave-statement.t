use Test;

plan 6;

{
    my $block = { leave 42; 23 };
    is $block(), 42, 'bare leave returns from the current block';
}

{
    my $block = { leave &?BLOCK: 42; 23 };
    is $block(), 42, 'indirect-object leave targets the current block';
}

{
    my @values = [1, do { leave; 2 }, 3];
    is @values, [1, 3], 'leave without args yields null list in list context';
}

{
    my $outer = sub () {
        my $inner = sub () {
            leave $outer: 42;
            23;
        };
        $inner();
        21;
    };
    is $outer(), 42, 'leave can target an outer sub reference';
}

{
    my $sub = sub () {
        my $inner = sub () {
            leave Sub: 42;
            23;
        };
        1000 + $inner();
    };
    is $sub(), 42, 'leave Sub: exits the caller routine';
}

{
    my $sub = sub () { do LABEL: { LABEL.leave(42) } + 1000 };
    is $sub(), 1042, 'labeled do block honors LABEL.leave';
}
