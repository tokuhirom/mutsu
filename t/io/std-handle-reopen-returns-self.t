use v6;
use Test;

# `$*OUT.open(...)` must not be routed through the ordinary "open the file at
# this path" code path. A standard handle's `path` attribute is the sentinel
# name "STDOUT" (`.path` reports it as `IO::Special.new("<STDOUT>")`), so
# resolving it as a filename created a real file called `STDOUT` in the CWD
# and - because `.open` writes the opened handle back over its receiver -
# silently redirected the process's own output into it. Rakudo re-applies the
# options it was given to the live stream and returns `self`. See #8428.

plan 11;

my $dir = $*TMPDIR.add("mutsu-std-reopen-{$*PID}");
$dir.mkdir;
LEAVE { try $dir.&{ .dir».unlink; .rmdir } }

my $reopened-out;
my $reopened-err;
my $reopened-in;
my @stray;

indir $dir, {
    $reopened-out = $*OUT.open: :w, :!out-buffer;
    $reopened-err = $*ERR.open: :w, :!out-buffer;
    $reopened-in  = $*IN.open;
    @stray = $dir.dir.map(*.basename).sort;
};

# The plan line and every result below reached the real stdout, which is the
# other half of the regression: a displaced $*OUT emits no TAP at all.
is @stray.elems, 0, 'reopening a standard handle creates no file in the CWD';

ok $reopened-out === $*OUT, '$*OUT.open returns the standard output handle itself';
ok $reopened-err === $*ERR, '$*ERR.open returns the standard error handle itself';
ok $reopened-in  === $*IN,  '$*IN.open returns the standard input handle itself';

is $reopened-out.path.Str, '<STDOUT>', 'the reopened handle is still the STDOUT stream';
ok $reopened-out.opened, 'the reopened standard handle reports as open';

# Options the caller did not pass keep the handle's current value (rakudo
# defaults each of them to the attribute rather than to a fresh default), and
# the ones that were passed are applied in place.
{
    # Measure first, restore, and only then report: while $*OUT.nl-out is not
    # "\n" the Test module's own output would not be line-separated either.
    my $saved = $*OUT.nl-out;
    $*OUT.nl-out = '|';
    $*OUT.open(:w);
    my $kept = $*OUT.nl-out;
    $*OUT.open(:w, :nl-out("!"));
    my $applied = $*OUT.nl-out;
    $*OUT.nl-out = $saved;
    is $kept, '|', '.open leaves an unmentioned :nl-out alone';
    is $applied, '!', '.open applies an explicit :nl-out to the live stream';
}

{
    my $saved = $*IN.chomp;
    $*IN.chomp = False;
    $*IN.open;
    my $kept = $*IN.chomp;
    $*IN.open(:chomp);
    my $applied = $*IN.chomp;
    $*IN.chomp = $saved;
    is $kept, False, '.open leaves an unmentioned :chomp alone';
    is $applied, True, '.open applies an explicit :chomp to the live stream';
}

# The Testo::Out::TAP shape that surfaced this: a class whose attributes are
# reopened standard streams. Under the bug the emitted lines went to a file.
{
    my class TapOut {
        has $.out = $*OUT.open: :w, :!out-buffer;
        method emit($line) { $.out.say: $line }
    }
    lives-ok { TapOut.new.emit('# reopened-handle attribute writes to stdout') },
        'a class attribute initialised from $*OUT.open writes to the real stdout';
}
