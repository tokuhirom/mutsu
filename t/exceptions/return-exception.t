use Test;

plan 7;

is(
    (sub f() { try { return 42 }; 99 })(),
    42,
    'return escapes try and returns to the caller',
);

{
    my $caught = False;
    sub f() {
        CATCH { default { $caught = True } }
        return 42;
    }
    is f(), 42, 'CATCH does not intercept return in a routine body';
    ok !$caught, 'routine CATCH block is not invoked for return';
}

{
    my $caught = False;
    sub f() {
        try {
            CATCH { default { $caught = True } }
            return 42;
        }
        return 99;
    }
    is f(), 42, 'CATCH inside try does not intercept return';
    ok !$caught, 'try CATCH block is not invoked for return';
}

{
    my $seen = '';
    sub f() {
        CONTROL { default { $seen = .^name } }
        return 42;
    }
    is f(), Nil, 'CONTROL can intercept return and suppress the value';
    is $seen, 'CX::Return', 'CONTROL sees return as CX::Return';
}
