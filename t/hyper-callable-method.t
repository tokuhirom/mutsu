use Test;

plan 3;

sub increment($value) { $value + 1 }
is-deeply (1, 2)».&increment, (2, 3), 'hyper call accepts a named sub';

my &double = -> $value { $value * 2 };
is-deeply (2, 3)».&double, (4, 6), 'hyper call accepts a callable variable';

my @seen;
for 2 {
    @seen.push($_);
    ($_ - 1,)».&?BLOCK if $_ > 0;
}
is-deeply @seen, [2, 1, 0], 'hyper call can recursively invoke an inline for block';
