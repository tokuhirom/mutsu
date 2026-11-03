use Test;
use JSON::Fast;

# Time a thread spends blocked in a native wait (sleep, a thread join, ...) is
# reported under its own `blocked` excluded region, not as `gc`: a run that
# collects nothing must not report GC time (#15577). Only structure is asserted,
# never a duration (ADR-0106 D5).
plan 5;

my $dir = $*TMPDIR.child("mutsu-profiler-blocked-{$*PID}");
$dir.mkdir;
my $script = $dir.child('fixture.raku');
my $json = $dir.child('prof.json');
LEAVE {
    try $script.unlink;
    try $json.unlink;
    try $dir.rmdir;
}

$script.spurt: q:to/RAKU/;
    my $t = start { sleep 0.3; 1 };
    my $x = 0;
    for 1..1000 { $x += $_ }
    await $t;
    say $x;
    RAKU

my $proc = run $*EXECUTABLE.absolute, "--profile=$json", $script.absolute,
    :out, :err, :env(|%*ENV, MUTSU_GC => 'off');
my $out = $proc.out.slurp(:close);
$proc.err.slurp(:close);
is $proc.exitcode, 0, 'the profiled run exits cleanly';
is $out, "500500\n", '... and prints what the program prints';

my %doc = from-json($json.slurp);
my @names = (%doc<excluded_regions> // []).map(*<region>);
ok 'blocked' (elem) @names, 'the awaited sleep is reported as blocked time';
nok 'gc' (elem) @names, '... and not as GC time when no collect ran';
ok %doc<excluded_regions>.grep(*<region> eq 'blocked').all<us> > 0, 'the blocked row carries a measured time';
