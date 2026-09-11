use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# A core routine whose parameters are ALL optional, used paren-less with no
# argument, is a real zero-arg call -- not the bareword *string*.
#
# The identifier-call parser's last resort turns an unrecognised word into
# `Expr::BareWord(name)`, so `sleep;` evaluated to the Str "sleep" and returned
# instantly, and `my $x = exit;` bound "exit" and let execution carry on. See
# `is_zero_arg_callable_builtin()` in src/parser/primary/ident/predicates.rs:
# membership is measured against Rakudo, which rejects a bare
# argument-requiring routine at compile time.
#
# Routines that genuinely need an argument must keep falling through to the
# bareword/X::Obsolete path, and calls with arguments must be untouched.

plan 36;

# --- the zero-arg call really happens ---------------------------------------

for <join sum min max minmax unique squish repeated flat zip cross roundrobin
     hash item slip sort undefine val> -> $name {
    my $got = EVAL("my \$x = $name; \$x");
    nok ($got ~~ Str:D && $got eq $name),
        "bare `$name` compiles to a zero-arg call, not the bareword string";
}

is (my $j = join), '', 'bare `join` returns join()\'s empty string';
is (my $s = sum), 0, 'bare `sum` returns sum()\'s 0';
is (my $h = hash).elems, 0, 'bare `hash` returns an empty Hash';
nok (my $m = min).defined, 'bare `min` returns an undefined value';

# --- `sleep;` blocks, it does not fall through ------------------------------

# `sleep` is `sub sleep($seconds = Inf --> Nil)`, so a bare `sleep` sleeps
# indefinitely. Run it in a child and assert it has NOT finished: a slow or
# loaded machine can only make this pass, never fail.
#
# Two seconds, not more: a `sleep` that falls through as a bareword returns in
# about 20ms, so this is already a hundredfold margin, and the wait is paid in
# full by the serial `prove t/` of the gc-stress and jit-stress jobs.
{
    my $prog = Proc::Async.new($*EXECUTABLE.absolute, '-e', 'sleep; print "RETURNED"');
    my $out = '';
    $prog.stdout.tap: { $out ~= $^a };
    my $promise = $prog.start;
    await Promise.anyof(Promise.in(2), $promise);
    my $finished = $promise.status == Kept;
    $prog.kill unless $finished;
    nok $finished, 'bare `sleep;` blocks instead of returning the word "sleep"';
}

# --- `exit` in expression position really exits -----------------------------

is_run 'sub f() { my $x = exit; print "STILL RUNNING" }; f(); print "AND HERE"',
    %(:out(''), :err(''), :status(0)),
    'bare `exit` in expression position exits instead of continuing';

# --- argument-requiring routines are unaffected -----------------------------

# `say` is checked as a bare statement and the Perl 5 unaries in expression
# position, which is where each of them is diagnosed today.
for 'say;', 'my $x = ord;', 'my $x = chr;', 'my $x = lc;', 'my $x = uc;',
    'my $x = abs;' -> $code {
    my $message;
    try {
        EVAL $code;
        CATCH { default { $message = .message } }
    }
    ok $message.defined && $message.contains('Unsupported use of bare'),
        "`$code` still reports Unsupported use of bare";
}

# --- real calls are unaffected ----------------------------------------------

is join('-', 1, 2, 3), '1-2-3', 'join with arguments still works';
is (join '-', 1, 2, 3), '1-2-3', 'join as a listop still works';
is min(3, 1, 2), 1, 'min with arguments still works';
is sum(1, 2, 3), 6, 'sum with arguments still works';
is sort(3, 1, 2).join(','), '1,2,3', 'sort with arguments still works';
is (flat 1, 2, 3).elems, 3, 'flat as a listop still works';
