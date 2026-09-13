use v6;
use Test;

plan 3;

role R {
    method who { 'base' }
}

role R[Int $n] does R {
    method n { $n }
}

class C does R[3] { }
my $instance = C.new;
is $instance.who, 'base',
    'a parameterized role can compose its same-named base candidate';
is $instance.n, 3,
    'the parameterized role still receives its bound argument';

throws-like 'role S does S { }', X::InvalidType,
    'an unparameterized role cannot compose its own candidate';

done-testing;
