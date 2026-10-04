use Test;

# A react block that dies rethrows the ORIGINAL exception with the
# `X::React::Died` role mixed in, as rakudo does -- not a new wrapper
# exception -- so `.message`/`.Str`/type checks still see the original and
# only `.gist` explains where the react died (#11650).

plan 13;

{
    my $e;
    try { react { whenever Promise.in(0) { die "boom" } }; CATCH { default { $e = $_ } } }
    is $e.^name, 'X::AdHoc+{X::React::Died}', 'the role is mixed into the original X::AdHoc';
    ok $e ~~ X::AdHoc, 'still an X::AdHoc';
    ok $e ~~ X::React::Died, 'does X::React::Died';
    ok $e.does(X::React::Died), '.does agrees';
    is $e.message, 'boom', '.message is the original message';
    is $e.Str, 'boom', '.Str is the original message';
    like $e.gist, /^ 'A react block:' .* 'Died because of the exception:' \n '    boom' /,
        '.gist explains the react death and indents the original';
}

{
    my class MyErr is Exception { method message { 'mine' } }
    my $e;
    try { react { whenever Promise.in(0) { MyErr.new.throw } }; CATCH { default { $e = $_ } } }
    ok $e ~~ MyErr, 'a user exception keeps its class';
    is $e.message, 'mine', '... and its message';
    like $e.gist, /'Died because of the exception:' \n '    mine'/, '... and its gist is wrapped';
}

{
    my $p = Promise.new;
    $p.break('nope');
    my $e;
    try { react { whenever $p { } }; CATCH { default { $e = $_ } } }
    is $e.^name, 'X::AdHoc+{X::React::Died}', 'a broken promise reason dies as X::AdHoc';
    is $e.message, 'nope', '... with the reason as its message';
}

{
    # Catching around the react by message, the way real code retries.
    my $tries = 0;
    loop {
        $tries++;
        react { whenever Promise.in(0) { die "connection refused" if $tries < 3 } }
        last;
        CATCH { default { next if .Str.contains('connection refused') } }
    }
    is $tries, 3, 'a CATCH matching on .Str around a react sees the original text';
}
