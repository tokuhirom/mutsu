use v6.*;
use Test;
use lib 't/lib';
use P5substr;

# P5substr exports a multi `substr` that returns a Proxy for lvalue use. Its
# name collides with mutsu's native `substr` fallback, so the imported routine
# must win before the fallback turns the call into a read-only Str result.

plan 5;

my $name = 'fred';
substr($name, 4) = 'dy';
is $name, 'freddy', 'imported substr lvalue writes through its Proxy';

my $other = '1234';
with substr($other, 1, 2) {
    $_ = 'a';
    is $other, '1a4', 'first Proxy assignment updates the source';
    $_ = 'xyz';
    is $other, '1xyz4', 'second Proxy assignment updates the source';
}

my $out-of-range = 'freddy';
dies-ok { substr($out-of-range, 7) = 'gap' },
    'an out-of-range imported substr lvalue throws';
is $out-of-range, 'freddy', 'a failed out-of-range store leaves the source unchanged';
