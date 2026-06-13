use Test;

# Awaiting a broken Promise throws an exception that does X::Await::Died (Raku
# mixes the X::Await::Died role into the original cause). The original message
# and, for a thrown exception, its backtrace are preserved.

plan 8;

# Broken with a plain string cause.
{
    my $p = Promise.new;
    $p.break("golly!");
    my $caught;
    try { await $p; CATCH { default { $caught = $_ } } }
    ok $caught.does(X::Await::Died), 'await of string-broken Promise does X::Await::Died';
    ok $caught ~~ Exception, 'and is an Exception';
    like ~$caught, /golly/, 'gist contains the original message';
}

# Broken by a die inside a start block: original exception type and backtrace
# are preserved while also doing X::Await::Died.
{
    sub deep-failer { die "kaboom" }
    my $p = start { deep-failer() };
    my $caught;
    try { await $p; CATCH { default { $caught = $_ } } }
    ok $caught.does(X::Await::Died), 'await of die-broken Promise does X::Await::Died';
    like ~$caught, /kaboom/, 'gist contains the original die message';
    like $caught.backtrace.Str, /deep\-failer/, 'backtrace keeps the throwing location';
}

# Awaiting several promises where one dies still surfaces X::Await::Died.
{
    my $ok = Promise.new; $ok.keep(1);
    my $bad = start { die "nope" };
    my $caught;
    try { await $ok, $bad; CATCH { default { $caught = $_ } } }
    ok $caught.does(X::Await::Died), 'await of a list with one broken does X::Await::Died';
    like ~$caught, /nope/, 'gist contains the failing promise message';
}
