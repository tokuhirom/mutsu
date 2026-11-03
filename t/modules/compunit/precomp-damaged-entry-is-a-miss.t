use Test;

# #14553: a precompilation entry damaged after it was written is a cache miss,
# not a process exit. The routine body of a lazily decoded entry used to fail on
# its first use, long after the module loaded, with status 70.

plan 6;

my $lib = $?FILE.IO.parent(2).sibling('lib').Str;
my $cache = $*TMPDIR.add("mutsu-damaged-precomp-{$*PID}");
my $script = $cache.add('probe.raku');

$cache.mkdir;
$script.spurt: qq:to/PROBE/;
    use lib '$lib';
    use PrecompBytecodeProbe;
    say probe();
    PROBE

sub run-probe() {
    my %env = %*ENV;
    %env<XDG_CACHE_HOME> = $cache.Str;
    my $proc = run($*EXECUTABLE, $script.Str, :out, :err, :%env);
    ($proc.exitcode, $proc.out.slurp(:close).chomp, $proc.err.slurp(:close))
}

my ($cold-code, $expected, $cold-err) = run-probe();
is $cold-code, 0, 'cold run succeeds' or diag $cold-err;

my @entries = $cache.add('mutsu').add('precomp').dir.grep(*.extension eq 'code');
ok @entries.elems >= 1, 'the cold run wrote a compiled entry';

# Damage the tail of every entry: the routine bodies are the last section.
for @entries -> $file {
    my $bytes = $file.slurp(:bin);
    my $at = $bytes.elems - 8;
    $bytes[$at] = $bytes[$at] +^ 0xFF;
    $file.spurt($bytes, :bin);
}

my ($code, $out, $err) = run-probe();
is $code, 0, 'a damaged entry does not end the process' or diag $err;
is $out, $expected, 'the module is recompiled and answers as before';
unlike $err, /damaged/, 'no "damaged routine body" report';

my ($code2, $out2, $err2) = run-probe();
is "$code2 $out2", "0 $expected", 'the next run, on the rewritten entry, is clean';

END { run('rm', '-rf', $cache.Str) if $cache.e }
