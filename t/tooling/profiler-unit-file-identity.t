use Test;
use JSON::Fast;

# #8719: one source file has ONE runtime identity.
#
# mutsu names a compilation unit twice over: the unit stamp every compiled
# chunk carries (`CompiledCode::source_file`, ADR-0106 Slice 0), and the env's
# `?FILE`, which is what a call frame, `Code.file`, `CallFrame.file` and a
# backtrace report. The mainline used to publish a `fs::canonicalize`d path to
# the first and the path as spelled on the command line to the second, so
# running a script from its own directory made one file both
# `/abs/dir/fixture.raku` and `fixture.raku` -- and a profile's line rows could
# not be joined to its callsite rows.
#
# `$?FILE` is deliberately NOT that identity: it is absolutified, which is the
# split rakudo itself has. Measured against rakudo 2026.x:
#
#     $ cd dir && raku fixture.raku
#     $?FILE       /abs/dir/fixture.raku     <- $*CWD-joined
#     Code.file    fixture.raku              <- as spelled
#
# Absolutified, note, not canonicalized: rakudo keeps `.` and `..` components
# and does not resolve symlinks, so this file pins that too.

plan 14;

my $dir = $*TMPDIR.child("mutsu-unit-file-{$*PID}");
$dir.mkdir;
my $nest = $dir.child('nest');
$nest.mkdir;
my $script = $dir.child('fixture.raku');
my $json = $dir.child('prof.json');

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
    sub report() {
        say "FILE\t" ~ $?FILE;
        say "CODE\t" ~ &work.file;
        say "FRAME\t" ~ CallFrame.new.file;
    }
    work(200);
    report();
    RAKU

LEAVE {
    try $script.unlink;
    try $json.unlink;
    try $nest.rmdir;
    try $dir.rmdir;
}

# Run mutsu with the fixture's own directory as the working directory, so the
# spelling the command line carries is genuinely different from the absolute
# path. That is exactly the case the two identities used to disagree on.
sub mutsu-in($cwd, *@args) {
    my $proc = run $*EXECUTABLE.absolute, |@args, :$cwd, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    ($proc.exitcode, $out, $err);
}

sub reported($out) {
    my %seen;
    for $out.lines.grep(*.contains("\t")) -> $line {
        my ($key, $value) = $line.split("\t", 2);
        %seen{$key} = $value;
    }
    %seen;
}

# --- the Raku-visible identities --------------------------------------------

my ($status, $out, $err) = mutsu-in($dir, 'fixture.raku');
is $status, 0, 'a script invoked by a bare relative name runs';
my %seen = reported($out);

is %seen<CODE>, 'fixture.raku',
    'Code.file is the path as spelled on the command line';
is %seen<FRAME>, 'fixture.raku',
    '... and so is CallFrame.file';
is %seen<FILE>, $script.absolute,
    '$?FILE is that same path absolutified against $*CWD';

# Absolutified, not canonicalized: `.` and `..` survive, exactly as rakudo
# spells them. Invoking through `nest/..` names the very same file a third way.
my ($dstatus, $dout) = mutsu-in($dir, './nest/../fixture.raku');
is $dstatus, 0, 'a script invoked through `.` and `..` components runs';
my %dot = reported($dout);
is %dot<CODE>, './nest/../fixture.raku',
    'Code.file keeps the dotted spelling verbatim';
is %dot<FILE>, $dir.absolute ~ '/./nest/../fixture.raku',
    '$?FILE prepends $*CWD without folding `.` or `..` away (rakudo does not either)';

# A unit with no file at all keeps its pseudo-name: there is nothing to resolve
# it against, and rakudo reports a bare `-e` here too.
my ($estatus, $eout) = mutsu-in($dir, '-e', 'sub f { }; say "FILE\t" ~ $?FILE; say "CODE\t" ~ &f.file;');
is $estatus, 0, 'a -e program runs';
my %e = reported($eout);
is %e<FILE>, '-e', '$?FILE for -e is `-e`, not a path under the current directory';
is %e<CODE>, '-e', '... and so is Code.file';

# --- the identity the profiler joins its tables on --------------------------

# The chunk stamp is not reachable from Raku, so read it where it is published:
# the profile document keys its file rows by the chunk's identity and its
# caller rows by the frame's. Before #8719 those were the two spellings above
# and a caller row could not be matched to the file row it belongs to.
($status, $out, $err) = mutsu-in($dir, "--profile=prof.json", '--profile-report=json', 'fixture.raku');
is $status, 0, 'the same script profiles';
ok $json.e, '... and wrote its document';

my %doc = from-json($json.slurp);
my @files = %doc<files>.list;
is @files.map(*.<path>).unique.elems, 1,
    'one file ran, so the document names exactly one file';
is @files[0]<path>, 'fixture.raku',
    'the chunk identity is the spelled path, the same string the frames carry';
