use v6.d;
use Test;

plan 1;

class CallableValue does Callable {
    method CALL-ME(Int:D $value) { $value * 2 }
}

my &callable = CallableValue.new;
is (1, 2, 3)>>.&callable.join(','), '2,4,6',
    'a hyper call invokes CALL-ME on a callable instance';
