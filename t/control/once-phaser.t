use Test;

plan 12;

{
    my @out;
    for 1..3 {
        once { @out.push: 'first' };
        @out.push: $_;
    }
    is-deeply @out, ['first', 1, 2, 3], 'once block runs only on first loop iteration';
}

{
    my $str = '';
    my $sub1 = { once { $str ~= '1' } };
    my $sub2 = { once { $str ~= '2' } };
    $sub1();
    $sub1();
    $sub2();
    $sub2();
    is $str, '12', 'each closure gets an independent once state';
}

{
    my $var;
    my $sub = sub ($x) { once { $var += $x } };
    nok $var.defined, 'once block is lazy until first execution';
    $sub(2);
    is $var, 2, 'once block executes on first call';
    $sub(3);
    is $var, 2, 'once block is skipped after first call';
}

{
    my $count = 0;
    for 1..3 {
        do { once { $count++ } };
    }
    is $count, 3, 'nested do blocks get a fresh once scope each iteration';
}

{
    my $entered;
    my $sub = {
        my $value = once { $entered++; 23 };
        $value //= 42;
        $value;
    };
    nok $entered.defined, 'once rvalue is lazy until first use';
    is $sub(), 23, 'once rvalue returns its computed value';
    is $sub(), 23, 'once rvalue caches its value';
    is $entered, 1, 'once rvalue body runs exactly once';
}

{
    my $entered;
    my $sub = { once { $entered++; Mu } };
    nok $sub().defined, 'once rvalue can cache Mu';
    $sub();
    is $entered, 1, 'once rvalue is not re-run after returning Mu';
}
