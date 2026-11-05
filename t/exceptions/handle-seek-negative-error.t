use Test;

# A seek to a negative position fails the way MoarVM's lseek wrapper does:
# an X::AdHoc "Failed to seek in filehandle: 22" (EINVAL), not a made-up class.

plan 8;

my $f = $*TMPDIR.add("mutsu-seek-neg-{$*PID}-{(^1000000).pick}");
$f.spurt('foobar');
LEAVE $f.unlink;

my $h = $f.open;
for (-4, SeekFromBeginning), (-4, SeekFromCurrent), (-40, SeekFromEnd) -> ($n, $whence) {
    try $h.seek($n, $whence);
    isa-ok $!, X::AdHoc, "seek $n from $whence.key() is an X::AdHoc";
    is ~$!, 'Failed to seek in filehandle: 22', "... with MoarVM's message";
}
is $h.tell, 0, 'a failed seek leaves the position alone';
$h.seek(2, SeekFromBeginning);
is $h.tell, 2, 'a valid seek still works';
$h.close;
