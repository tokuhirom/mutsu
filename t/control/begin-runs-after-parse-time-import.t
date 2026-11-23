use v6;
use Test;

# A `BEGIN` of the program's top level that calls a routine an earlier `use`
# imports runs when the parser reaches it, after that import, as on rakudo
# (ADR-17558 §7.16): `BEGIN plan N` prints its plan before a later syntax
# error is reported, a `use` after it still loads at parse time, and an `exit`
# in it ends the program before the rest is parsed. Each case runs a child
# process so the order of its output is visible; every expectation is what
# rakudo prints.

plan 11;

sub run-code(Str $code) {
    my $proc = run $*EXECUTABLE, '-I', 't/lib', '-e', $code, :out, :err;
    (exitcode => $proc.exitcode, out => $proc.out.slurp(:close), err => $proc.err.slurp(:close))
}

{
    my %r = run-code(q:to/CODE/);
        use Test;
        BEGIN plan 3;
        1 +;
        CODE
    ok %r<out>.starts-with("1..3\n"), 'BEGIN plan runs before a later syntax error';
    isnt %r<exitcode>, 0, 'the syntax error still fails the program';
}

{
    my %r = run-code(q:to/CODE/);
        use Test;
        BEGIN { plan 1; skip-rest "why"; exit }
        1 +;
        CODE
    is %r<out>, "1..1\nok 1 - # SKIP why\n", 'an exit in a parse-time BEGIN ends the program there';
    is %r<exitcode>, 0, 'with its status';
    nok %r<err>.contains('SORRY'), 'the rest of the program is not parsed';
}

{
    my %r = run-code(q:to/CODE/);
        use Test;
        BEGIN plan 2;
        use LoadsAtParseTime;
        ok 1;
        ok 1;
        CODE
    is %r<out>, "1..2\nLoadsAtParseTime loaded\nok 1 - \nok 2 - \n",
        'a use after BEGIN plan still loads at parse time, after the plan';
}

{
    my %r = run-code(q:to/CODE/);
        BEGIN say 0;
        use Test;
        BEGIN plan 1;
        use LoadsAtParseTime;
        pass;
        CODE
    is %r<out>, "0\n1..1\nLoadsAtParseTime loaded\nok 1 - \n", 'BEGINs and loads keep source order';
}

{
    my %r = run-code(q:to/CODE/);
        use LoadsAtParseTime;
        BEGIN say loads-at-parse-time();
        1 +;
        CODE
    is %r<out>, "LoadsAtParseTime loaded\nimported\n",
        'a BEGIN calls a routine a module loaded at parse time exports';
}

{
    my %r = run-code(q:to/CODE/);
        use LoadsAtParseTime;
        BEGIN say loads-at-parse-time();
        say "mainline";
        CODE
    is %r<out>, "LoadsAtParseTime loaded\nimported\nmainline\n",
        'the import still holds for the mainline';
    is %r<exitcode>, 0, 'and the program succeeds';
}

{
    my %r = run-code(q:to/CODE/);
        use Test;
        BEGIN plan 2;
        BEGIN pass "at begin";
        pass "at run time";
        CODE
    is %r<out>, "1..2\nok 1 - at begin\nok 2 - at run time\n",
        'Test state set at parse time carries into the run';
}
