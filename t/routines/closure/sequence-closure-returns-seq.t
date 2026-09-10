use Test;

plan 7;

my $sequence = (1, * + 1 ... 4);
is $sequence.^name, 'Seq', 'finite closure sequence is a Seq';
ok $sequence ~~ Seq, 'finite closure sequence does Seq';
nok $sequence ~~ Array, 'finite closure sequence is not an Array';
nok $sequence.is-lazy, 'finite closure sequence is not lazy';

sub steps(--> Seq) { (1, * + 1 ... 4) }
is steps().List, (1, 2, 3, 4), 'Seq return constraint accepts finite closure sequence';

is ((1, * + 1 ... 4) X* 10).raku, '(10, 20, 30, 40).Seq',
    'cross operator forces a finite closure sequence';
is ((1, * + 1 ... 4) Z* (10, 20, 30, 40)).raku, '(10, 40, 90, 160).Seq',
    'zip operator still consumes a finite closure sequence';
