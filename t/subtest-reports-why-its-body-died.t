use Test;
plan 2;

# A subtest whose body dies is reported as a failure, but the reason used to be
# dropped on the floor: all the reader saw was `1..0` and a bare `not ok`. That
# made a dying subtest one of the hardest failures in the suite to diagnose —
# the vendored Cro suite's `http-middleware.rakutest` subtest 6 sat at a silent
# `1..0` until the interpreter was taught to say "Expected IO::Handle".
#
# The reason now goes to stderr (`$*ERR`), so TAP counting on stdout is
# untouched.

my $mutsu = $*EXECUTABLE.absolute;

sub run-snippet(Str $code) {
    my $file = $*TMPDIR.add("subtest-diag-{$*PID}-{(^10000).pick}.raku");
    $file.spurt($code);
    LEAVE { try $file.unlink }
    my $proc = run $mutsu, $file.absolute, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    return %( :$out, :$err );
}

my %dying = run-snippet(q:to/CODE/);
    use Test;
    plan 1;
    subtest {
        die "the body exploded";
    }, 'dies';
    CODE

ok %dying<err>.contains('the body exploded'),
    'a dying subtest reports what killed it on stderr';

my %living = run-snippet(q:to/CODE/);
    use Test;
    plan 1;
    subtest {
        plan 1;
        ok 1, 'inner';
    }, 'lives';
    CODE

nok %living<err>.contains('subtest died'),
    'a subtest whose body completes reports no death';
