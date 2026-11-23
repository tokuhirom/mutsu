use v6.e.PREVIEW;
use Test;

# A slang loaded at parse time has its productions run by the program's own
# interpreter (ADR-17558 slice 4b): an action runs when the parser reaches the
# production, as on rakudo, so what it does to the slang module's state is
# already there for a `BEGIN` later in the unit, and the run sees each
# production's effect once. Each case runs a child process (a `use lib` would
# keep the `BEGIN` in the prologue).

plan 3;

sub run-code(Str $code) {
    my $proc = run $*EXECUTABLE, '-I', 't/lib', '-e', $code, :out, :err;
    %(exitcode => $proc.exitcode, out => $proc.out.slurp(:close), err => $proc.err.slurp(:close))
}

{
    my %r = run-code(q:to/CODE/);
        use v6.e.PREVIEW;
        use Test;
        use ActionEffectSlang;
        remember alpha;
        BEGIN say collected();
        remember beta;
        say collected();
        CODE
    is %r<out>, "[alpha]\n[alpha beta]\n", 'a BEGIN after a production sees what its action did, the run each once';
}

{
    my %r = run-code(q:to/CODE/);
        use v6.e.PREVIEW;
        use Test;
        use ActionEffectSlang;
        remember alpha;
        BEGIN say collected();
        1 +;
        CODE
    is %r<out>, "[alpha]\n", 'the action ran before a later syntax error';
}

{
    my %r = run-code(q:to/CODE/);
        use v6.e.PREVIEW;
        use Test;
        use ActionEffectSlang;
        for ^3 { remember gamma }
        say collected().elems;
        CODE
    is %r<out>, "1\n", 'a production in a loop body ran its action once';
}
