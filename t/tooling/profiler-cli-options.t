use Test;
use JSON::Fast;

# ADR-0106 D8: the profiler's command-line surface, and what each way of
# getting it wrong does.
#
# Two different failure modes meet here, and they are deliberately different
# (both measured against rakudo 2026.07):
#
#   * an option mutsu does not know at all is an *option-list* error -- the
#     message and the usage go to stderr and the process exits **0**
#     (ADR-0017, `t/tooling/cli-option-errors.t`);
#   * a profiler option whose *value* names something mutsu does not implement
#     is rakudo's `Unknown profiler specified` on stderr with exit **1**.

plan 29;

my $dir = $*TMPDIR.child("mutsu-profiler-cli-{$*PID}");
$dir.mkdir;
my $script = $dir.child('fixture.raku');
$script.spurt: q:to/RAKU/;
    sub twice($n) { $n * 2 }
    my $t = 0;
    for ^50 { $t = $t + twice($_) }
    say $t;
    RAKU

LEAVE {
    try $script.unlink;
    for $dir.dir { try .unlink }
    try $dir.rmdir;
}

sub mutsu(*@args, :$cwd) {
    my $proc = $cwd
        ?? run($*EXECUTABLE.absolute, |@args, :out, :err, :cwd($cwd))
        !! run($*EXECUTABLE.absolute, |@args, :out, :err);
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err);
}

# --- values mutsu does not implement ----------------------------------------

# `heap` is a real rakudo profiler kind and mutsu does not do heap profiling, so
# the spelling is declined rather than quietly treated as something else.
my ($status, $out, $err) = mutsu('--profile', '--profile-kind=heap', $script.absolute);
is $status, 1, 'an unimplemented --profile-kind exits 1';
is $err, "Unknown profiler specified\n", '... with rakudo\'s message, on stderr';
is $out, '', '... and the program never runs';

($status, $out, $err) = mutsu('--profile', '--profile-kind=bogus', $script.absolute);
is $status, 1, 'and so does a nonsense one';
is $err, "Unknown profiler specified\n", '... reported identically';

($status, $out, $err) = mutsu('--profile', '--profile-report=xml', $script.absolute);
is $status, 1, 'an unimplemented --profile-report exits 1';
like $err, /'Unknown profiler report specified'/, '... naming the report option';

($status, $out, $err) = mutsu('--profile', '--profile-rate=0', $script.absolute);
is $status, 1, 'a rate outside the sampler range exits 1';
like $err, /'Invalid profiler rate'/, '... rather than being clamped silently';

($status, $out, $err) = mutsu('--profile', '--profile-rate=nine', $script.absolute);
is $status, 1, 'and so does a rate that is not a number';

($status, $out, $err) = mutsu('--profile', '--profile-jit=maybe', $script.absolute);
is $status, 1, 'an unrecognised --profile-jit exits 1';
like $err, /'Invalid profiler JIT setting'/, '... naming the setting';

# --- an option mutsu does not know at all -----------------------------------

($status, $out, $err) = mutsu('--profile-frobnicate', $script.absolute);
is $status, 0, 'an unknown profiler-shaped option is an option error, so exit 0';
ok $err.starts-with("Illegal option --profile-frobnicate\n"),
    '... reported the way every other unknown option is (ADR-0017)';

# --- the halves of the document ---------------------------------------------

my $lines-only = $dir.child('lines.json');
($status, $out, $err) = mutsu(
    "--profile=$lines-only", '--profile-kind=line', '--profile-report=json', $script.absolute);
is $status, 0, '--profile-kind=line runs';
is $out, "2450\n", '... and does not disturb the program';
my %doc = from-json($lines-only.slurp);
ok %doc<files>:exists, '... reporting the per-line half';
# Absent rather than present-and-empty: an empty table would read as "no routine
# ran", which is a different claim from "this run did not collect routines".
nok %doc<routines>:exists, '... and omitting the routine half entirely';
is %doc<header><kind>, 'line', '... and the header says which half it is';

my $routines-only = $dir.child('routines.json');
mutsu("--profile=$routines-only", '--profile-kind=routine', '--profile-report=json',
      $script.absolute);
%doc = from-json($routines-only.slurp);
ok %doc<routines>:exists, '--profile-kind=routine reports the routine half';
nok %doc<files>:exists, '... and omits the per-line half';

# --- the text report --------------------------------------------------------

my $unwanted = $dir.child('never-written.json');
($status, $out, $err) = mutsu("--profile=$unwanted", '--profile-report=text', $script.absolute);
is $status, 0, '--profile-report=text runs';
nok $unwanted.e, '... writes no file';
like $err, /'a text-only report writes no file'/,
    '... and says so rather than silently dropping the name';
# The text report has to be good enough to make HTML optional (ADR-0106 D7), so
# the four sections a triage session needs are all there.
#
# Either heading counts, and that is the point rather than a hedge: a fixture
# this small can finish between two ticks of a 1000Hz timer, in which case the
# line table is ranked by exact hits and says so. Asserting only the sampled
# heading would have been an assertion that a sample was taken -- the flaky test
# ADR-0106 D5 forbids, and it duly failed on the release binary after passing on
# the debug one.
like $err, /'TOP ' ['SELF LINES' | 'LINES BY HITS']/,
    '... the line table is on screen';
like $err, /'TOP ROUTINES (inclusive)'/, '... the top routines too';
like $err, /'SAMPLED'/,
    '... and the report says of itself that its times are sampled';

# --- the default file name --------------------------------------------------

($status, $out, $err) = mutsu('--profile', $script.absolute, cwd => $dir.absolute);
ok $dir.child('mutsu-prof.json').e,
    'a bare --profile writes mutsu-prof.json in the working directory';
like $err, /'Writing profiler output to'/, '... and names it on stderr';
