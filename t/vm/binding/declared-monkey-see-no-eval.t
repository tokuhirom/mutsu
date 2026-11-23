use Test;

# In rakudo the MONKEY-SEE-NO-EVAL pragma is the lexical `&MONKEY-SEE-NO-EVAL`:
# the regex interpolation check calls the routine visible there at compile
# time and goes by its answer. A routine the program declares counts too
# (#18748). `use Test` itself exports `MONKEY-SEE-NO-EVAL`, so each case runs
# in its own process.

plan 5;

sub run-code(Str $code) {
    my $p = run $*EXECUTABLE, '-e', $code, :out, :err;
    my $out = $p.out.slurp(:close).trim;
    $p.err.slurp(:close);
    $out
}

my $probe = q[my $c = 0; my $s = q<{$c++; "<alpha>"}>; try { "abc" ~~ /<$s>/ }; print $! ?? "refused" !! $c];

is run-code('sub MONKEY-SEE-NO-EVAL { 1 }; ' ~ $probe), '1',
    'a declared routine answering True lifts the check';
is run-code('sub MONKEY-SEE-NO-EVAL { 0 }; ' ~ $probe), 'refused',
    'one answering False keeps it';
is run-code('my $x = 0; sub MONKEY-SEE-NO-EVAL { $x }; $x = 1; ' ~ $probe), 'refused',
    'it is called before the variable it reads is assigned';
is run-code('my &MONKEY-SEE-NO-EVAL = sub { 1 }; ' ~ $probe), 'refused',
    'a variable holding a routine does not count';
is run-code('{ my sub MONKEY-SEE-NO-EVAL { 1 }; ' ~ $probe ~ ' }'), '1',
    'a lexical my sub counts in its block';
