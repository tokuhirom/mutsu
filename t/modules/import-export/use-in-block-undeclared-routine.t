use v6;
use Test;

# A `use` below the unit's top level imports into its own statement list: a call
# outside that list to a routine it imported is `Undeclared routine` at CHECK
# time, before anything runs, as on rakudo (#13441). Calls inside the list keep
# working. Each case runs a child process; the expectations are rakudo's.

plan 8;

sub run-code(Str $code) {
    my $proc = run $*EXECUTABLE, '-I', 't/lib', '-e', $code, :out, :err;
    %(exitcode => $proc.exitcode, out => $proc.out.slurp(:close), err => $proc.err.slurp(:close))
}

{
    my %r = run-code(q:to/CODE/);
        { use LoadsAtParseTime; }
        say "ran";
        loads-at-parse-time();
        CODE
    nok %r<out>.contains('ran'), 'nothing runs';
    ok %r<err>.contains('Undeclared routine'), 'the call outside the block is Undeclared routine';
    ok %r<err>.contains('loads-at-parse-time'), 'naming the routine';
    isnt %r<exitcode>, 0, 'the compile fails';
}

{
    my %r = run-code(q:to/CODE/);
        { use LoadsAtParseTime; say loads-at-parse-time() }
        CODE
    is %r<out>, "LoadsAtParseTime loaded\nimported\n", 'a call inside the block is fine';
}

{
    my %r = run-code(q:to/CODE/);
        sub f { use LoadsAtParseTime; loads-at-parse-time() }
        say f();
        CODE
    is %r<out>, "LoadsAtParseTime loaded\nimported\n", 'a call inside the routine body is fine';
}

{
    my %r = run-code(q:to/CODE/);
        { use LoadsAtParseTime; my &g = &loads-at-parse-time; say g() }
        CODE
    is %r<out>, "LoadsAtParseTime loaded\nimported\n", 'a &name inside the block is fine';
}

{
    my %r = run-code(q:to/CODE/);
        use LoadsAtParseTime;
        say loads-at-parse-time();
        CODE
    is %r<out>, "LoadsAtParseTime loaded\nimported\n", 'a top-level use still imports unit-wide';
}
