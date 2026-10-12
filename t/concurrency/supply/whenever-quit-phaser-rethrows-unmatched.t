use Test;

# Like CATCH, a whenever's QUIT phaser handles the quit only when a
# `when`/`default` matches; otherwise the exception is rethrown and quits the
# enclosing react/supply block (#12789). Cro's HTTP client relies on this: its
# request QUIT matches only two of its own exception types.

plan 8;

my $broken = Promise.new;
$broken.break('boom');

{
    try { react { whenever $broken { QUIT { when X::NYI { } } } } }
    ok $! ~~ X::React::Died, 'react: an unmatched `when` in QUIT rethrows';
    is $!.message, 'boom', '... the original exception';
}

{
    my $seen = False;
    try { react { whenever $broken { QUIT { $seen = True } } } }
    ok $seen, 'react: a QUIT phaser without when/default still runs';
    ok $! ~~ X::React::Died, '... and the exception is rethrown';
}

{
    try { react { whenever $broken { QUIT { default { } } } } }
    nok $!, 'react: `default` handles the quit';
}

{
    my $p = Promise(supply { whenever $broken { QUIT { when X::NYI { } } } });
    try await $p;
    is $p.status, Broken, 'supply: an unmatched QUIT breaks the Promise';
}

{
    my $s = Supplier.new;
    my $p = Promise(supply { whenever $s.Supply { QUIT { when X::NYI { } } } });
    $s.quit(X::AdHoc.new(payload => 'live'));
    try await $p;
    is $p.cause.message, 'live', 'supply: a live source quit an unmatched QUIT rethrows';
}

{
    my $s = Supplier.new;
    my $p = Promise(supply { whenever $s.Supply { QUIT { when X::AdHoc { emit 'caught' } } } });
    $s.quit(X::AdHoc.new(payload => 'live'));
    is await($p), 'caught', 'supply: a matching `when` handles a live source quit';
}
