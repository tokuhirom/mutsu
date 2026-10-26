use Test;

plan 6;

# rakudo 2026.09 runs a bare block under `while` / `until` on every pass.

{
    my $i = 0;
    my @called;
    { @called.push($i) } while $i++ < 2;
    is-deeply @called, [1, 2], 'while modifier invokes a bare Block operand';
    is $i, 3, 'while modifier still evaluates its condition';
}

{
    my $i = 0;
    dies-ok { { say $^x } while $i++ < 2 }, 'a placeholder block is called without arguments';
    is $i, 1, 'and dies on the first pass';
}

{
    my $i = 0;
    my @called;
    { @called.push($i) } until $i++ >= 2;
    is-deeply @called, [1, 2], 'until modifier invokes a bare Block operand';
    is $i, 3, 'until modifier still evaluates its condition';
}
