use v6;
use Test;

# A `use` loads its module when the parser reaches it, in the interpreter that
# runs the unit, and the run-time `use` only imports (ADR-17558): the module
# runs once, before anything the importer's later parse decides. Each case runs
# a child process so the order of its output is visible; every expectation is
# what rakudo prints.

plan 6;

sub run-code(Str $code) {
    my $proc = run $*EXECUTABLE, '-I', 't/lib', '-e', $code, :out, :err;
    (exitcode => $proc.exitcode, out => $proc.out.slurp(:close), err => $proc.err.slurp(:close))
}

{
    my %r = run-code(q:to/CODE/);
        say "mainline";
        use LoadsAtParseTime;
        use LoadsAtParseTime;
        { use LoadsAtParseTime; }
        say loads-at-parse-time();
        CODE
    is %r<out>, "LoadsAtParseTime loaded\nmainline\nimported\n",
        'the module runs once, before the mainline, however often it is used';
}

{
    # The syntax error comes after the `use`: the module has already run when
    # the parser reports it.
    my %r = run-code(q:to/CODE/);
        use LoadsAtParseTime;
        my $x = ;
        CODE
    is %r<out>, "LoadsAtParseTime loaded\n", 'the module runs before a later syntax error is found';
    isnt %r<exitcode>, 0, 'the syntax error still fails the program';
}

{
    # A module that dies while loading fails the importer's compile at the
    # `use`: nothing of the importer's mainline runs.
    my %r = run-code(q:to/CODE/);
        say "mainline";
        use LoadsAtParseTimeDies;
        CODE
    is %r<out>, "", 'a module that dies while loading stops the compile';
    like %r<err>, /'LoadsAtParseTimeDies died while loading'/, 'the module\'s own error is reported';
}

{
    # A `BEGIN` written before the `use` runs before the module.
    my %r = run-code(q:to/CODE/);
        BEGIN say "begin";
        use LoadsAtParseTime;
        CODE
    is %r<out>, "begin\nLoadsAtParseTime loaded\n", 'a BEGIN before the use runs first';
}
