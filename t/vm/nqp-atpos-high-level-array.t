use v6;
use nqp;
use Test;

plan 7;

my @a = 1, 2;

# A high-level Array is not a VMArray: the positional nqp ops die on it,
# exactly as rakudo does.
throws-like { nqp::atpos(@a, 0) }, Exception,
    message => /'does not support positional operations'/,
    'nqp::atpos on an Array dies';
throws-like { nqp::bindpos(@a, 0, 5) }, Exception,
    message => /'does not support positional operations'/,
    'nqp::bindpos on an Array dies';
is @a.join(','), '1,2', 'the Array was left untouched';

# Its $!reified storage, and nqp::list, are VMArrays and keep working.
my $r = nqp::getattr(@a, List, '$!reified');
is nqp::atpos($r, 1), 2, 'atpos on $!reified reads the Array elements';
nqp::bindpos($r, 0, 9);
is @a[0], 9, 'bindpos on $!reified writes through to the Array';

my $l = nqp::list(5, 6);
is nqp::atpos($l, 1), 6, 'atpos on nqp::list works';
nqp::bindpos($l, 2, 7);
is nqp::elems($l), 3, 'bindpos on nqp::list grows it';
