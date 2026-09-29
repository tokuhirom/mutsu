use Test;

plan 5;

{
    my $i = 0;
    my @called;
    { @called.push($i) } while $i++ < 2;
    is-deeply @called, [], 'while modifier does not invoke a bare Block operand';
    is $i, 3, 'while modifier still evaluates its condition';
}

{
    my $i = 0;
    { say $^x } while $i++ < 2;
    is $i, 3, 'an uncalled modifier Block may carry a placeholder signature';
}

{
    my $i = 0;
    my @called;
    { @called.push($i) } until $i++ >= 2;
    is-deeply @called, [], 'until modifier does not invoke a bare Block operand';
    is $i, 3, 'until modifier still evaluates its condition';
}
