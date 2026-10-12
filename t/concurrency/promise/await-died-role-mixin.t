use Test;

# `await` of a broken promise rethrows the cause `but X::Await::Died` (#12843):
# the role shows in `.^name`, a bare reason becomes an X::AdHoc first, and the
# role's gist names where the promise was awaited.

plan 12;

{
    my $p = Promise.new;
    $p.break(X::AdHoc.new(payload => 'x'));
    try await $p;
    is $!.^name, 'X::AdHoc+{X::Await::Died}', 'an exception cause keeps its type, role mixed in';
    ok $! ~~ X::Await::Died, 'it does X::Await::Died';
    ok $! ~~ X::AdHoc, 'and is still an X::AdHoc';
}

{
    my $p = Promise.new;
    $p.break('s');
    try await $p;
    is $!.^name, 'X::AdHoc+{X::Await::Died}', 'a Str cause is wrapped in X::AdHoc';
    is $!.message, 's', '.message is the reason';
    like $!.gist, /^ 'An operation first awaited:' \n '  in block ' .* \n \n
                    'Died with the exception:' \n '    s' \n/,
        'gist explains the await';
}

{
    my class MyEx is Exception { method message { 'mine' } }
    my $p = start { MyEx.new.throw };
    try await $p;
    is $!.^name, 'MyEx+{X::Await::Died}', 'a user exception keeps its class';
    is $!.message, 'mine', 'and its message';
}

{
    sub awaiter { await start { die 'zz' } }
    try awaiter;
    like $!.gist, /'An operation first awaited:' \n '  in sub awaiter'/,
        'the gist header is the await site';
    unlike $!.backtrace.Str, /awaiter/, 'the backtrace stays the throw location';
}

{
    try await Promise.kept(1).then({ die 'in then' });
    is $!.^name, 'X::AdHoc+{X::Await::Died}', 'a .then callback death';
}

{
    try await Promise.new.keep(1), start { die 'nope' };
    ok $! ~~ X::Await::Died, 'one broken promise among several';
}
