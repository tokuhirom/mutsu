use Test;

# Promise keep/break/vow semantics, established against rakudo 2026.06.
#
# Everything here is bounded: every wait is an `await` on a promise this file
# itself resolves (or that dies immediately), and no test sleeps. In
# particular the vow-protection case does NOT use `Promise.in(10)` to keep the
# promise pending -- holding the vow is what keeps it pending, and the test
# resolves it through the vow at the end.

plan 41;

# ---------------------------------------------------------------------------
# .result on a Broken promise rethrows the cause with X::Promise::Broken
# mixed in. `.cause` keeps handing back the plain, unmixed original.
# ---------------------------------------------------------------------------

{
    my $p = Promise.new;
    $p.break('oh no');
    is $p.status, Broken, 'break() leaves the promise Broken';

    my $ex = try { $p.result } // $!;
    is $ex.^name, 'X::AdHoc+{X::Promise::Broken}',
        '.result throws the cause with X::Promise::Broken mixed in';
    ok $ex ~~ X::Promise::Broken, 'the thrown exception does X::Promise::Broken';
    ok $ex ~~ X::AdHoc, 'it is still an X::AdHoc';
    ok $ex ~~ Exception, 'it is still an Exception';
    is $ex.Str, 'oh no', 'the mixin does not change .Str';
    is $ex.message, 'oh no', 'the mixin does not change .message';
    is $ex.payload, 'oh no', 'the X::AdHoc payload is the break reason';

    # .cause is the *un*mixed original -- in rakudo it is not even the same
    # object as the one .result throws.
    is $p.cause.^name, 'X::AdHoc', '.cause reports the plain cause type';
    nok $p.cause ~~ X::Promise::Broken, '.cause does NOT carry the role';
    is $p.cause.Str, 'oh no', '.cause stringifies to the break reason';
}

# A user exception keeps its own type under the mixin.
{
    my class MyEx is Exception { method message { 'my ex msg' } }
    my $p = Promise.new;
    $p.break(MyEx.new);
    is $p.cause.^name, 'MyEx', '.cause of a user exception keeps its type';
    my $ex = try { $p.result } // $!;
    is $ex.^name, 'MyEx+{X::Promise::Broken}',
        '.result mixes the role into a user exception too';
    is $ex.Str, 'my ex msg', 'the user message survives the mixin';
}

# A non-exception reason is wrapped in X::AdHoc.
{
    my $p = Promise.new;
    $p.break(42);
    is $p.cause.^name, 'X::AdHoc', 'a non-exception reason is wrapped in X::AdHoc';
    is $p.cause.Str, '42', 'the wrapped reason stringifies to the reason';
}

# The role overrides .gist -- and only .gist -- to explain the rethrow.
{
    my $p = Promise.new;
    $p.break('some reason');
    my $ex = try { $p.result } // $!;
    my $gist = $ex.gist;
    ok $gist.starts-with('Tried to get the result of a broken Promise'),
        '.gist leads with the broken-Promise explanation';
    ok $gist.contains('Original exception:'),
        '.gist chains to the original exception';
    ok $gist.contains('some reason'),
        '.gist still shows the original reason';
    is $ex.Str, 'some reason', '.Str is unaffected by the gist override';
}

# ---------------------------------------------------------------------------
# Vow protection. Taking the vow -- whether explicitly, or implicitly via
# .keep/.break -- consumes it; every later attempt through the Promise is
# X::Promise::Vowed.
# ---------------------------------------------------------------------------

my $vowed-message = 'Access denied to keep/break this Promise; already vowed';

{
    # The documented shape, minus the 10-second timer: the vow holder, not the
    # Promise, is what may resolve it.
    my ($promise, $vow) = do {
        my $p = Promise.new;
        ($p, $p.vow);
    };

    my $ex = try { $promise.keep; Nil } // $!;
    is $ex.^name, 'X::Promise::Vowed', '.keep through a vowed Promise throws X::Promise::Vowed';
    is $ex.Str, $vowed-message, 'X::Promise::Vowed says access is denied';
    is $ex.message, $vowed-message, '.message agrees with .Str';

    is (try { $promise.break('x'); Nil } // $!).^name, 'X::Promise::Vowed',
        '.break through a vowed Promise throws X::Promise::Vowed';
    is (try { $promise.vow; Nil } // $!).^name, 'X::Promise::Vowed',
        'a second .vow throws X::Promise::Vowed';

    is $promise.status, Planned, 'none of the denied attempts resolved it';

    # The vow holder still can, which is the whole point of the protection.
    $vow.keep(42);
    is $promise.status, Kept, 'the vow holder can still keep it';
    is $promise.result, 42, 'and the kept value arrives';
}

{
    # .keep consumes the vow itself, so a second .keep is Vowed, not Resolved.
    my $p = Promise.new;
    $p.keep('first');
    is (try { $p.keep('second'); Nil } // $!).^name, 'X::Promise::Vowed',
        'a second .keep on the same Promise is X::Promise::Vowed';
    is $p.result, 'first', 'the first keep still stands';
}

{
    # Promise.kept/.broken hand back a settled promise whose vow was never
    # taken -- so resolving it again is Resolved, and .vow still works. Each
    # case needs its OWN promise: the failed .keep below consumes the vow.
    my $ex = try { Promise.kept(3).keep(9); Nil } // $!;
    is $ex.^name, 'X::Promise::Resolved',
        '.keep on Promise.kept is X::Promise::Resolved (its vow was never taken)';
    is $ex.Str, 'Cannot keep/break a Promise more than once (status: Kept)',
        'X::Promise::Resolved names the status';
    ok (try { Promise.kept(3).vow; True }), '.vow on Promise.kept still succeeds';
}

{
    # A promise the runtime resolves itself is internally vowed.
    my $started = Promise.start({ 7 });
    await $started;
    is (try { $started.keep(9); Nil } // $!).^name, 'X::Promise::Vowed',
        'Promise.start hands back an internally vowed Promise';
    is $started.result, 7, 'the start block result is intact';
}

# .cause is only valid on a Broken promise.
{
    my $p = Promise.new;
    $p.keep(1);
    my $ex = try { $p.cause; Nil } // $!;
    is $ex.^name, 'X::Promise::CauseOnlyValidOnBroken',
        '.cause on a Kept promise throws X::Promise::CauseOnlyValidOnBroken';
    is $ex.Str, 'Can only call cause on a broken promise (status: Kept)',
        'the CauseOnlyValidOnBroken message names the status';
    is (try { Promise.new.cause; Nil } // $!).^name, 'X::Promise::CauseOnlyValidOnBroken',
        '.cause on a Planned promise throws too';
}

# ---------------------------------------------------------------------------
# Promise.cause's backtrace must not repeat a frame.
#
# mutsu's callframe model is not rakudo's (fewer frames, no Raku CORE
# setting), so this asserts the *absence of duplication* structurally rather
# than pinning rakudo's exact frame list.
# ---------------------------------------------------------------------------

{
    my $p = Promise.start({ die 'Broken Promise' });
    try await $p;
    is $p.status, Broken, 'a dying start block breaks its Promise';

    my @lines = $p.cause.backtrace.Str.lines.grep(*.trim.chars);
    ok @lines.elems >= 1, 'the cause carries at least one backtrace frame';
    is @lines.elems, @lines.unique.elems,
        'no backtrace frame is repeated verbatim in Promise.cause';
}
