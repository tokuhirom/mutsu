use v6;
use Test;

plan 3;

sub replace($value is copy) {
    $value = 'replacement';
    $value
}

sub relay($value) {
    replace($value)
}

sub capture-and-relay(\value) {
    relay(value)
}

is capture-and-relay('original'), 'replacement',
    'is copy detaches a value received through a sigilless capture';

my $original = 'original';
is capture-and-relay($original), 'replacement',
    'is copy does not write through the sigilless capture chain';
is $original, 'original', 'the caller value remains unchanged';
