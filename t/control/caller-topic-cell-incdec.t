use v6.c;
use Test;

plan 1;

my $block = {
    my sub bump { $CALLER::_++ }
    $_ = 42;
    bump();
    $_
};
is $block(), 43, 'CALLER::_ increment reads and writes a captured topic cell';
