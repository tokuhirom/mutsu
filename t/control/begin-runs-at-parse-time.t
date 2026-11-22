use v6;
use Test;

# A `BEGIN` of the program's top level runs when the parser reaches it,
# interleaved with the `use` loads in source order (ADR-17558 slice 5), so a
# `use` after it still loads at parse time and the rest of the parse reads the
# loaded unit. Each case runs a child process so the order of its output is
# visible; every expectation is what rakudo prints.

plan 17;

sub run-code(Str $code) {
    my $proc = run $*EXECUTABLE, '-I', 't/lib', '-e', $code, :out, :err;
    (exitcode => $proc.exitcode, out => $proc.out.slurp(:close), err => $proc.err.slurp(:close))
}

{
    # The module computes its operator names while it loads, which only the
    # loaded unit can tell the parse.
    my %r = run-code(q:to/CODE/);
        BEGIN say "begin";
        use ComputedStashLoud;
        say 5ZZ;
        CODE
    is %r<out>, "begin\nComputedStashLoud loaded\n5 ZZ\n",
        'a use after a BEGIN still reads the unit it loaded';
}

{
    my %r = run-code(q:to/CODE/);
        BEGIN %*ENV<MUTSU_BEGIN_AT_PARSE_TAG> = "the BEGIN";
        use LoadsReadingBeginEnv;
        CODE
    is %r<out>, "loaded with the BEGIN\n", 'what a BEGIN does is seen by the module a later use loads';
}

{
    my %r = run-code(q:to/CODE/);
        BEGIN say 1;
        say 4;
        use LoadsAtParseTime;
        BEGIN say 3;
        CODE
    is %r<out>, "1\nLoadsAtParseTime loaded\n3\n4\n", 'BEGINs and use loads run in source order';
}

{
    my %r = run-code(q:to/CODE/);
        BEGIN say "begin";
        my $x = ;
        CODE
    is %r<out>, "begin\n", 'a BEGIN runs before a later syntax error is found';
    isnt %r<exitcode>, 0, 'the syntax error still fails the program';
}

{
    my %r = run-code(q:to/CODE/);
        say "mainline";
        BEGIN die "boom";
        CODE
    is %r<out>, "", 'a BEGIN that dies stops the compile';
    like %r<err>, /boom/, 'its error is reported';
    isnt %r<exitcode>, 0, 'and fails the program';
}

{
    # A BEGIN that reads the unit's lexicals sees them in their static state.
    my %r = run-code(q:to/CODE/);
        my $c = True;
        BEGIN say $c.raku;
        my $x;
        BEGIN $x = 5;
        say $x;
        CODE
    is %r<out>, "Any\n5\n", 'a BEGIN reading a lexical still sees its static state';
}

{
    # One that reads a lexical keeps its place among the BEGIN-time effects:
    # the BEGIN and the use after it do not run ahead of it.
    my %r = run-code(q:to/CODE/);
        my $x;
        BEGIN { $x = 1; say "first" }
        BEGIN say "second";
        use LoadsAtParseTime;
        say $x;
        CODE
    is %r<out>, "first\nsecond\nLoadsAtParseTime loaded\n1\n", 'the order holds after a BEGIN reading a lexical';
}

{
    my %r = run-code(q:to/CODE/);
        sub helper { "helper" }
        BEGIN say helper();
        CODE
    is %r<out>, "helper\n", 'a BEGIN calling a routine of the unit sees it';
}

{
    # Effects the prologue runs keep their order before a later BEGIN.
    my %r = run-code(q:to/CODE/);
        constant X = do { say "constant"; 1 };
        my $v will begin { say "will begin" };
        BEGIN say "begin";
        CODE
    is %r<out>, "constant\nwill begin\nbegin\n", 'a computed constant and a will begin still run first';
}

{
    # A routine of the unit that shadows a core one is what the BEGIN calls.
    my %r = run-code(q:to/CODE/);
        sub say(|) { print "mine\n" }
        BEGIN say "core";
        CODE
    is %r<out>, "mine\n", 'a BEGIN calls the unit\'s own routine of a core name';
}

{
    my %r = run-code(q:to/CODE/);
        BEGIN say "begin";
        use LoadsAtParseTimeDies;
        CODE
    is %r<out>, "begin\n", 'a BEGIN before a module that dies while loading has run';
}

{
    # A BEGIN that writes a dynamic variable runs at parse time too, so the
    # module a later `use` loads sees it before a later syntax error stops the
    # compile.
    my %r = run-code(q:to/CODE/);
        BEGIN %*ENV<MUTSU_BEGIN_AT_PARSE_TAG> = "the dynamic BEGIN";
        use LoadsReadingBeginEnv;
        say 1 +;
        CODE
    is %r<out>, "loaded with the dynamic BEGIN\n",
        'a BEGIN writing %*ENV runs, and a use after it loads, before a later syntax error';
}

{
    # The unit's own dynamic variable is in its frame, in its static state.
    my %r = run-code(q:to/CODE/);
        my $*BEGIN-AT-PARSE = 5;
        BEGIN say $*BEGIN-AT-PARSE;
        CODE
    is %r<out>, "(Any)\n", "a BEGIN reads the unit's own dynamic variable in its static state";
}

{
    my %r = run-code(q:to/CODE/);
        BEGIN say $*CU.^name;
        CODE
    is %r<out>, "RakuAST::CompUnit\n", 'a BEGIN reads the compile-time $*CU';
}
