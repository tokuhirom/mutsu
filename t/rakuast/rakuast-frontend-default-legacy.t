use Test;

# ADR-10723 Stage 3: the default frontend is `parse -> RakuAST -> lower`;
# MUTSU_LEGACY=1 opts out to the parser's tree. The two differ in the tree
# `--dump-ast` prints (the lowered tree versus the parser's own), and both run
# the program to the same result.

plan 9;

my $dir = $*TMPDIR.add("mutsu-frontend-default-$*PID");
$dir.mkdir;
LEAVE { try { .unlink for $dir.dir; $dir.rmdir } }
my $file = $dir.add('prog.raku');
$file.spurt: "my \$x = 1 with 3;\nsay \$x;\n";

sub run-with(*@args, *%env) {
    my %e = %*ENV;
    %e<MUTSU_LEGACY>:delete;
    %e<MUTSU_RAKUAST>:delete;
    %e{.key} = .value for %env;
    my $p = run $*EXECUTABLE, |@args, :out, :err, :env(%e);
    $p.out.slurp(:close)
}

my $default = run-with('--dump-ast', $file.absolute);
my $legacy = run-with('--dump-ast', $file.absolute, MUTSU_LEGACY => '1');
my $zero = run-with('--dump-ast', $file.absolute, MUTSU_RAKUAST => '0');
my $one = run-with('--dump-ast', $file.absolute, MUTSU_RAKUAST => '1');
my $both = run-with('--dump-ast', $file.absolute, MUTSU_LEGACY => '1', MUTSU_RAKUAST => 'all');

ok $default.chars > 0, 'the default frontend dumps a tree';
isnt $default, $legacy, 'MUTSU_LEGACY=1 selects the parser tree, not the default';
is $zero, $legacy, 'MUTSU_RAKUAST=0 is the old spelling of the opt-out';
is $one, $default, 'MUTSU_RAKUAST=1 is the default';
is $both, $legacy, 'MUTSU_LEGACY=1 wins over MUTSU_RAKUAST';

is run-with($file.absolute), "1\n", 'the default frontend runs the program';
is run-with($file.absolute, MUTSU_LEGACY => '1'), "1\n", 'the legacy frontend runs it too';
is run-with('-e', 'say EVAL q[1 + 2]'), "3\n", 'EVAL runs under the default frontend';
is run-with('-e', 'say EVAL q[1 + 2]', MUTSU_LEGACY => '1'), "3\n", 'EVAL runs under the legacy frontend';
