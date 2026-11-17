use v6;
use Test;

# A `use` loads its module when the parser reaches it, and the rest of the
# importer's parse reads what the loaded unit provides (ADR-17558 §2.2):
# routines whose names the module computes while it loads exist for the
# parse, under the tags the `use` imports, and the module runs once. Each
# case runs a child process so the module's own output is visible; every
# expectation is what rakudo prints.

plan 5;

sub run-code(Str $code) {
    my $proc = run $*EXECUTABLE, '-I', 't/lib', '-e', $code, :out, :err;
    (exitcode => $proc.exitcode, out => $proc.out.slurp(:close), err => $proc.err.slurp(:close))
}

{
    my %r = run-code(q:to/CODE/);
        use ComputedStashLoud;
        say 5ZZ;
        say 6YY;
        CODE
    is %r<out>, "ComputedStashLoud loaded\n5 ZZ\n6 YY\n",
        'computed operator names parse, and the module runs once';
}

{
    my %r = run-code(q:to/CODE/);
        use ComputedStashLoud;
        say 5XX;
        CODE
    isnt %r<exitcode>, 0, 'a computed name under a tag the use does not import is not an operator';
}

{
    my %r = run-code(q:to/CODE/);
        use ComputedStashLoud :extra;
        say 5XX;
        CODE
    is %r<out>, "ComputedStashLoud loaded\n5 XX\n", 'the tag the use names imports its computed names';
}

{
    my %r = run-code(q:to/CODE/);
        { use ComputedStashLoud; say 7ZZ; }
        CODE
    is %r<out>, "ComputedStashLoud loaded\n7 ZZ\n", 'a use inside a block reads the loaded unit too';
}

{
    # `Str.AST` compiles its source, so its `use` loads the module there too.
    my %r = run-code(q:to/CODE/);
        use experimental :rakuast;
        use MONKEY-SEE-NO-EVAL;
        say EVAL(q[use ComputedStashLoud; 8ZZ].AST);
        CODE
    is %r<out>, "ComputedStashLoud loaded\n8 ZZ\n", 'Str.AST reads the unit its use loaded';
}
