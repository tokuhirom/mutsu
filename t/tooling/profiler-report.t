use Test;
use JSON::Fast;

# ADR-0106 Slice 5: the profiler's output -- the `--profile` CLI surface, the
# JSON document (docs/profiler.md) and the text summary.
#
# Everything asserted here is a **count or a structure**: the exact hit count of
# a line whose trip count is known by construction, which routine the caller
# table links to which callsite, which fields the header carries. ADR-0106 D5
# forbids asserting a duration or a sample count anywhere -- the sampler's
# numbers are a function of how fast this machine ran, so a test that asserted
# one would be a flaky test by construction.

plan 32;

my $dir = $*TMPDIR.child("mutsu-profiler-{$*PID}");
$dir.mkdir;
my $script = $dir.child('fixture.raku');
my $json = $dir.child('prof.json');

# The two body lines run exactly TRIPS times; the `while` condition runs once
# more, the time it is false. That makes line 5 the hottest line in the file, by
# construction rather than by measurement.
my $TRIPS = 500;
$script.spurt: q:to/RAKU/;
    sub work($n) {
        my $s = 0;
        my $i = 0;
        while $i < $n {
            $s = $s + $i;
            $i = $i + 1;
        }
        return $s;
    }
    say work(500);
    RAKU

LEAVE {
    try $script.unlink;
    try $json.unlink;
    try $dir.rmdir;
}

sub mutsu(*@args) {
    my $proc = run $*EXECUTABLE.absolute, |@args, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err);
}

# --- the document -----------------------------------------------------------

my ($status, $out, $err) = mutsu("--profile=$json", $script.absolute);
is $status, 0, 'a profiled run exits like an unprofiled one';
is $out, "124750\n", '... and prints exactly what the program prints';
ok $err.contains("Writing profiler output to $json"),
    '... naming the file it wrote, the way rakudo does';
ok $json.e, '... and the file is there';

my %doc = from-json($json.slurp);
is %doc<mutsu_prof_version>, 1, 'the document names its schema version';

my %header = %doc<header>;
ok %header<mutsu_version>.defined, 'the header carries the mutsu version';
ok %header<argv>.elems >= 2, '... the argv it was invoked with';
ok %header<jit> eq 'on' | 'off', '... the JIT state the program ran in';
ok %header<gc> eq 'on' | 'off', '... the GC state';
is %header<kind>, 'both', '... the profile kind';
is %header<time_is_sampled>, True,
    '... and, loudly, that its times are sampled and not a measurement';
is %header<blocked_threads_absent>, True,
    '... and that a thread parked in a native call is absent, not idle';
is %header<sampling><rate_hz>, 1000, 'the default sampling rate is 1000Hz';
is %header<sampling><tick>, 'timer', '... from a timer thread';

# ADR-0106 gate 3, the one the ADR asks to see asserted in t/: the hot line is
# the hot line, and its hit count is the trip count exactly.
my @files = %doc<files>.list;
is @files.elems, 1, 'one file ran, so the document has one file row';
is @files[0]<path>, $script.absolute, '... named by its path';

my %hits = @files[0]<lines>.map({ .<line> => .<hits> });
is %hits{5}, $TRIPS, 'the loop body line ran exactly the trip count';
is %hits{6}, $TRIPS, '... and so did the second body line';
is %hits{4}, $TRIPS + 1, '... while the condition ran once more';
# `hits` counts *line entries* (the `op_lines[ip] != last_line` edge, ADR-0106
# D5), so a line that calls a routine is entered twice: once to make the call,
# and once more when control comes back to finish the statement. Documented in
# docs/profiler.md, and asserted here so it stays deliberate.
is %hits{10}, 2, '... and the calling line is entered twice, call and return';

# The caller breakdown (ADR-0106 D3) -- the column a flat line table cannot
# produce.
my @work = %doc<routines>.grep({ .<name> eq 'work' });
is @work.elems, 1, 'the routine table names `work` once';
is @work[0]<entries>, 1, '... entered exactly once';
my @callers = @work[0]<callers>.list;
is @callers.elems, 1, '... called from exactly one place';
is @callers[0]<line>, 10, '... the line that holds the call';
is @callers[0]<calls>, 1, '... exactly once';

# A line the counters saw but the sampler did not carries **no** self_us key
# rather than a zero: a zero that means "not measured" is a lie a tool reads as
# data. Which lines those are is a timing question, so what is asserted is the
# invariant -- a sampled line is always a counted line, never the reverse.
my $fabricated = @files[0]<lines>.grep({ .<self_us>:exists and not .<hits>:exists }).elems;
is $fabricated, 0, 'no line carries sampled time without having been counted';

# --- a call made inside a module belongs to the module ----------------------
#
# A frame records its call site's file as the dynamically scoped `?FILE`, which
# still names the mainline while a `use`d module's routine is running (#8719).
# Taken at face value that files a module's callsites under the *script's* path
# -- with the module's line numbers, so a caller row names a line the script
# does not have. The profiler resolves the call site's file from the enclosing
# body instead, and this is what pins it.
my $lib = $dir.child('lib');
$lib.mkdir;
$lib.child('ProfFixture.rakumod').spurt: q:to/RAKU/;
    unit module ProfFixture;
    sub leaf($n) { $n + 1 }
    our sub outer($n) is export {
        my $t = 0;
        for ^3 { $t = $t + leaf($n) }
        $t;
    }
    RAKU
my $user = $dir.child('user.raku');
$user.spurt: qq:to/RAKU/;
    use lib '{$lib.absolute}';
    use ProfFixture;
    say outer(2);
    RAKU
my $modjson = $dir.child('module.json');
($status, $out, $err) = mutsu("--profile=$modjson", '--profile-report=json', $user.absolute);
is $status, 0, 'a script that uses a module profiles';
is $out, "9
", '... and still prints what it printed';

my %mdoc = from-json($modjson.slurp);
my @leaf = %mdoc<routines>.grep({ .<name> eq 'leaf' });
is @leaf.elems, 1, 'the module routine is in the table';
is @leaf[0]<entries>, 3, '... entered once per trip';
my $caller = @leaf[0]<callers>[0];
ok $caller<file>.contains('ProfFixture.rakumod'),
    '... and its caller is in the module it was called from, not the script';
is $caller<line>, 5, '... at the line the call is actually on';
LEAVE {
    try $lib.child('ProfFixture.rakumod').unlink;
    try $lib.rmdir;
    try $user.unlink;
    try $modjson.unlink;
}
