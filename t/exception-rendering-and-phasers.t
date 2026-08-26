use v6;
use Test;

# Pins for three exception-rendering gaps fixed together:
#
#  * an uncaught exception is rendered with `.gist`, so a `method gist`
#    override decides what stderr shows (it used to print the message and
#    backtrace unconditionally, ignoring every override);
#  * `Exception.gist` is message + backtrace for a class only the MRO
#    identifies as an exception (`class E is Exception`), and `.backtrace`
#    is answerable on one;
#  * a compile-time diagnosis's `$!.backtrace` is a real `Backtrace` whose
#    `.is-runtime` is False, not an unaskable placeholder;
#  * `X::Phaser::PrePost` quotes the failed condition's verbatim source text.
#
# An uncaught exception cannot be observed in-process, so those cases run in a
# child process and assert on its stderr and exit code.

plan 21;

sub run-snippet($code) {
    run $*EXECUTABLE, '-e', $code, :err, :out;
}

# --- uncaught rendering goes through .gist -----------------------------------

{
    my $p = run-snippet 'class E is Exception {
        method message { "the-msg" }
        method gist    { "THE-GIST" }
    }
    E.new.throw;';
    is $p.exitcode, 1, 'an uncaught exception exits 1';
    is $p.err.slurp(:close).lines.join("\n"), 'THE-GIST',
        'uncaught rendering uses .gist, not .message';
    is $p.out.slurp(:close), '', 'nothing reaches stdout';
}

{
    # No `gist` override: the default rendering is still message + backtrace.
    my $p = run-snippet 'class E is Exception { method message { "only-msg" } }
    E.new.throw;';
    my $err = $p.err.slurp(:close);
    like $err, /^ 'only-msg' \n /, 'the default gist opens with the message';
    like $err, /'in block <unit>'/, 'and still carries a backtrace';
}

{
    # `.gist` that dies must not replace the user's error with the secondary
    # one; the renderer falls back to the message-and-backtrace form.
    my $p = run-snippet 'class E is Exception {
        method message { "fallback-msg" }
        method gist    { die "gist blew up" }
    }
    E.new.throw;';
    is $p.exitcode, 1, 'a dying .gist still exits 1';
    like $p.err.slurp(:close), /'fallback-msg'/,
        'a dying .gist falls back to the message';
}

{
    # An unhandled Failure renders both stacks: the fail site and the throw
    # site. Routing through .gist must not lose the second one.
    my $p = run-snippet qq:to/SNIPPET/;
    sub risky() \{ fail "nope" }
    my \$v = risky();
    say "unrelated";
    say \$v + 1;
    SNIPPET
    my $err = $p.err.slurp(:close);
    like $err, /^ 'nope' /, 'an unhandled Failure reports its message';
    like $err, /'Actually thrown at:'/,
        'and keeps the dual fail-site/throw-site backtrace';
}

# --- Exception.gist / .backtrace on a user Exception subclass ----------------

{
    class E is Exception { method message { "usermsg" } }
    try { E.new.throw };
    is $!.Str, 'usermsg', 'Exception.Str is the bare message';
    like $!.gist, /^ 'usermsg' \n \s* 'in block' /,
        'Exception.gist is the message plus the backtrace';
    is $!.backtrace.^name, 'Backtrace', 'a thrown user exception has a Backtrace';
    ok $!.backtrace.is-runtime, 'and it is a runtime backtrace';

    my $unthrown = E.new;
    is $unthrown.gist, 'usermsg', 'an unthrown exception gists to just its message';
    nok $unthrown.backtrace.defined, 'and has no backtrace';
}

# --- a compile-time diagnosis carries a real, non-runtime Backtrace ----------

{
    try { EVAL q[my $0] };
    is $!.backtrace.^name, 'Backtrace',
        'a compile-time diagnosis backtrace is a Backtrace, not a Str';
    nok $!.backtrace.is-runtime, 'and reports is-runtime False';
}

# --- X::Phaser::PrePost quotes the condition's source text ------------------

{
    sub pre-block($x) { PRE { $x ~~ Int } };
    try { pre-block "foo" };
    is $!.condition, '{ $x ~~ Int }',
        'a failed PRE names its condition verbatim, braces included';
    is $!.Str, q[Precondition '{ $x ~~ Int }' failed],
        'and the message quotes it';
}

{
    sub post-block($x) { POST { $x ~~ Int }; return "s" };
    try { post-block "foo" };
    is $!.Str, q[Postcondition '{ $x ~~ Int }' failed],
        'a failed POST quotes its condition too';
}

{
    sub multi-line($x) {
        PRE {
            $x ~~ Int
            && $x > 3
        }
    };
    try { multi-line "foo" };
    is $!.condition, "\{\n            \$x ~~ Int\n            && \$x > 3\n        \}",
        'a multi-line condition keeps its own line breaks and indentation';
}

done-testing;
