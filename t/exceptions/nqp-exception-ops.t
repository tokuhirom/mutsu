use Test;
use nqp;

# The `nqp::` exception-handling ops (#11497). Expected values were checked
# against `raku -e 'use nqp; ...'`.

plan 28;

# nqp::die / nqp::die_s reach a Raku handler as `die` does: an X::AdHoc whose
# payload is the message.
try nqp::die("boom");
is $!.^name, 'X::AdHoc', 'nqp::die throws an X::AdHoc';
is $!.message, 'boom', 'nqp::die carries its message';
is $!.payload, 'boom', 'nqp::die carries its message as the payload';
try nqp::die_s("boom2");
is $!.message, 'boom2', 'nqp::die_s throws its message';
{
    my $r = nqp::die("in expr");
    CATCH { default { is .message, 'in expr', 'CATCH sees nqp::die in an expression' } }
}

# A blank VM exception object and its setters.
my $e := nqp::newexception();
is $e.^name, 'BOOTException', 'nqp::newexception makes a BOOTException';
ok nqp::isnull(nqp::getpayload($e)), 'its payload starts out null';
is nqp::getextype($e), 0, 'its category starts out 0';
is nqp::setmessage($e, "m"), 'm', 'nqp::setmessage returns the message';
is nqp::getmessage($e), 'm', 'nqp::getmessage reads it back';
nqp::setpayload($e, 42);
is nqp::getpayload($e), 42, 'nqp::setpayload / nqp::getpayload round-trip';
nqp::setextype($e, 4);
is nqp::getextype($e), 4, 'nqp::setextype / nqp::getextype round-trip';

# nqp::throw: a message-only exception is an X::AdHoc of the message; a Raku
# exception payload is thrown as itself.
my $f := nqp::newexception();
nqp::setmessage($f, "thrown");
try nqp::throw($f);
is $!.^name, 'X::AdHoc', 'nqp::throw of a message raises an X::AdHoc';
is $!.message, 'thrown', '... with that message';
my $g := nqp::newexception();
nqp::setpayload($g, X::Str::Numeric.new(source => "a", pos => 0, reason => "r"));
try nqp::throw($g);
is $!.^name, 'X::Str::Numeric', 'nqp::throw of a Raku exception payload throws it';

# nqp::exception: the exception the running handler handles, null outside one.
try {
    die "inner";
    CATCH {
        default {
            my $ex := nqp::exception();
            is $ex.^name, 'BOOTException', 'nqp::exception is a BOOTException in CATCH';
            is nqp::getpayload($ex).message, 'inner', '... whose payload is the Raku exception';
            ok nqp::isnull_s(nqp::getmessage($ex)), '... and whose message is null';
            is nqp::getextype($ex), 1, '... and whose category is CATCH';
        }
    }
}
ok nqp::isnull(nqp::exception()), 'nqp::exception is null outside a handler';
sub peek { nqp::getpayload(nqp::exception()).message }
try { die "deep"; CATCH { default { is peek(), 'deep', 'a routine called from a handler sees its exception' } } }

# nqp::rethrow re-raises the handled exception as itself.
try { try { die X::Str::Numeric.new(source => "a", pos => 0, reason => "r"); CATCH { default { nqp::rethrow(nqp::exception()) } } } }
is $!.^name, 'X::Str::Numeric', 'nqp::rethrow re-raises the original exception';

# nqp::backtracestrings: one line per frame of the thrown exception.
sub thrower { nqp::die("bt") }
try {
    thrower();
    CATCH {
        default {
            my $lines := nqp::backtracestrings(nqp::exception());
            ok nqp::elems($lines) > 0, 'nqp::backtracestrings lists the frames';
            ok nqp::eqat(nqp::atpos($lines, 0), '   at ', 0), '... the first one "   at"';
        }
    }
}

# Control categories drive the control-exception machinery.
my @seen;
for 1..3 {
    my $c := nqp::newexception();
    nqp::setextype($c, nqp::const::CONTROL_NEXT);
    nqp::throw($c) if $_ == 2;
    @seen.push($_);
}
is @seen, [1, 3], 'nqp::throw of CONTROL_NEXT skips an iteration';
my $sum = 0;
for 1..5 {
    my $c := nqp::newexception();
    nqp::setextype($c, nqp::const::CONTROL_LAST);
    nqp::throw($c) if $_ == 3;
    $sum += $_;
}
is $sum, 3, 'nqp::throw of CONTROL_LAST leaves the loop';
my @taken = gather {
    my $t := nqp::newexception();
    nqp::setextype($t, nqp::const::CONTROL_TAKE);
    nqp::setpayload($t, 7);
    nqp::throw($t);
    take 8;
};
is @taken, [7, 8], 'nqp::throw of CONTROL_TAKE takes its payload';

# A CONTROL_WARN is resumable; nqp::resume continues after the throw.
my @log;
{
    my $w := nqp::newexception();
    nqp::setextype($w, nqp::const::CONTROL_WARN);
    nqp::setmessage($w, "nw");
    nqp::throw($w);
    @log.push('resumed');
    CONTROL {
        default {
            @log.push(.^name ~ ' ' ~ .message);
            @log.push(nqp::getextype(nqp::exception()));
            nqp::resume(nqp::exception());
        }
    }
}
is @log, ['CX::Warn nw', 256, 'resumed'], 'a thrown CONTROL_WARN reaches CONTROL and resumes';
