use Test;

# A resumable warning raised by a *native* coercion (`Int.Numeric`, `Nil.Str`,
# ...) used to be signalled by returning `warn_signal_with_resume` and letting it
# unwind. That is wrong twice over: the resume value rides in the error's
# `return_value`, which the enclosing routine boundary applies as an explicit
# `return`, and a `CONTROL { when CX::Warn { ... .resume } }` handler further up
# has no raise site left to resume into. The warning is now settled where it is
# raised. roast's `Test::Util` leans on exactly this shape in `warns-like`.

plan 8;

sub caught-by-control(&code) {
    my ($did, $msg, $reached) = False, '', False;
    code();
    $reached = True;
    CONTROL { when CX::Warn { $did = True; $msg = .message; .resume } }
    ($did, $msg, $reached);
}

# An explicit `warn` already worked; keep it pinned as the reference shape.
my ($did, $msg, $reached) = caught-by-control { warn "boom" };
ok $did, 'an explicit warn reaches the CONTROL handler';
is $msg.lines[0], 'boom', '... with its message';
ok $reached, '... and the statement after the raise still runs';

# A native numeric coercion on a bare type object.
($did, $msg, $reached) = caught-by-control { Int.Numeric };
ok $did, 'a native numeric coercion reaches the CONTROL handler';
is $msg, 'Use of uninitialized value of type Int in numeric context',
    '... with rakudo\'s wording';
ok $reached, '... and the statement after the raise still runs';

# The coercion still resumes with its value when nothing handles the warning.
my $n = quietly Int.Numeric;
is-deeply $n, 0, 'an unhandled numeric coercion resumes with 0';

# A user class that composes Numeric without defining its own .Numeric warns
# and resumes with .new -- the same raise site, reached through the interpreter.
my class CustomNumeric does Numeric { method new { 42 } }
($did, $msg, $reached) = caught-by-control { CustomNumeric.Numeric };
ok $did && $reached,
    'a role-composed Numeric type object warns and resumes at the raise site';
