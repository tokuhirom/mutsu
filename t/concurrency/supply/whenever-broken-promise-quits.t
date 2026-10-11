use Test;

# A `whenever` over a Broken promise quits the enclosing supply block with the
# promise's cause, unless the whenever's QUIT phaser handles it (#12789). The
# Promise of such a supply used to wait out a 30s deadline and then be Kept.

plan 11;

my $broken = Promise.new;
$broken.break('boom');

sub elapsed-ok($start, $what) { ok now - $start < 10, "$what resolves promptly" }

{
    my $t = now;
    my $p = Promise(supply { whenever $broken -> $x { emit $x } });
    try await $p;
    is $p.status, Broken, 'Promise(supply) over an already-broken promise is Broken';
    is $p.cause.message, 'boom', '... with the promise cause';
    elapsed-ok $t, 'already-broken source';
}

{
    my $late = Promise.new;
    my $t = now;
    my $p = supply { whenever $late -> $x { emit $x } }.Promise;
    start { sleep 0.1; $late.break('later') };
    try await $p;
    is $p.status, Broken, 'a promise broken later quits the supply too';
    elapsed-ok $t, 'later-broken source';
}

{
    sub stage() {
        supply {
            my $c = Promise.new;
            start { sleep 0.1; $c.break('nested') };
            whenever $c -> $x { emit $x }
        }
    }
    my $t = now;
    my $p = Promise(supply { whenever stage() -> $x { emit $x } });
    try await $p;
    is $p.cause.message, 'nested', 'a nested stage whose promise breaks quits the outer supply';
    elapsed-ok $t, 'nested stage';
}

{
    my $p = Promise(supply {
        whenever $broken -> $x { emit $x; QUIT { default { emit 'handled: ' ~ .message } } }
    });
    is await($p), 'handled: boom', 'a QUIT phaser sees the cause as an exception and handles it';
}

{
    my $seen;
    react { whenever $broken { QUIT { default { $seen = .^name } } } }
    is $seen, 'X::AdHoc', 'react: a QUIT phaser on a promise source receives an X::AdHoc';
}

{
    my $p = Promise(supply { whenever $broken { QUIT { default { emit 'q'; done } } } });
    is await($p), 'q', 'done in the QUIT phaser keeps the promise with the last emitted value';
}

{
    my $kept = Promise.kept(42);
    is await(Promise(supply { whenever $kept -> $x { emit $x * 2 } })), 84,
        'a kept promise source still emits its result';
}
