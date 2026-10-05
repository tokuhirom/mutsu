use Test;

plan 5;

my $dir = $*TMPDIR.child("bench-det-warmup-{$*PID}");
$dir.mkdir;
my $mock = $dir.child('mutsu');
my $log = $dir.child('calls.log');
$mock.spurt: q:to/SHELL/;
    #!/bin/sh
    printf '%s\t%s\t%s\n' "$MUTSU_JIT" "$BENCH_DET" "$1" >> "$WARM_LOG"
    SHELL
$mock.chmod(0o755);

LEAVE {
    try $log.unlink;
    try $mock.unlink;
    try $dir.rmdir;
}

my %env = %*ENV, MUTSU_BIN => $mock.Str, WARM_LOG => $log.Str;
my $proc = run('bash', 'scripts/bench-det.sh', '--warmup',
    'benchmarks/bench-hash.raku', :out, :err, :%env);
my $out = $proc.out.slurp(:close);
my $err = $proc.err.slurp(:close);
is $proc.exitcode, 0, 'warmup-only mode succeeds without valgrind';
is $out, '', 'warmup does not write benchmark rows';
ok $err.contains('warming benchmarks/bench-hash.raku'), 'warmup identifies the benchmark';
my @calls = $log.lines;
is @calls.elems, 2, 'each JIT configuration runs once';
is @calls.join("\n"), "off\t1\tbenchmarks/bench-hash.raku\non\t1\tbenchmarks/bench-hash.raku",
    'both runs use BENCH_DET and the requested benchmark';
