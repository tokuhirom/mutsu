use Test;

# A bare block's implicit rw topic aliases a caller variable even when that
# variable is itself named `$_`. The two call frames have distinct topic
# bindings, and the block's final value must reach the caller's binding.
plan 4;

my &increment = { $_++ };

sub copy-topic($_ is copy) { increment($_); $_ }
is copy-topic(1), 2, 'a routine topic receives a bare block writeback';

{
    my $_ = 5;
    increment($_);
    is $_, 6, 'a lexical topic receives a bare block writeback';
}

sub copy-named($value is copy) { increment($value); $value }
is copy-named(1), 2, 'an ordinary scalar caller still receives writeback';

is [1, 2].map(-> $_ is copy { increment($_); $_ }).join(','),
    '2,3', 'a pointy block topic receives a nested bare block writeback';
