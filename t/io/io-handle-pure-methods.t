use Test;

# Pure-handle IO::Handle methods (tell/eof/seek/opened/close/t) — resolved
# VM-native via the shared IoHandleState methods (③ native IO PR-C). These must
# behave identically to the interpreter's native fork.

plan 16;

my $path = $*TMPDIR.child("mutsu-io-pure-{$*PID}.txt");
$path.spurt("abcdefghij\nklmno\n");   # 17 bytes

my $fh = $path.open;
ok $fh.opened, 'opened on a fresh handle is True';
is $fh.tell, 0, 'tell starts at 0';
nok $fh.eof, 'eof is False at start';
is $fh.get, 'abcdefghij', 'get reads the first line';
is $fh.tell, 11, 'tell advances past the first line';

$fh.seek(0, SeekFromBeginning);
is $fh.tell, 0, 'seek from beginning resets to 0';

$fh.seek(3, SeekFromCurrent);
is $fh.tell, 3, 'seek from current advances';

$fh.seek(-1, SeekFromEnd);
is $fh.tell, 16, 'seek from end with negative offset';
nok $fh.eof, 'eof is False just before the last byte';

$fh.seek(0, SeekFromEnd);
ok $fh.eof, 'eof is True at end of file';

ok $fh.close, 'close returns True the first time';
nok $fh.opened, 'opened is False after close';

# tell on a non-file handle ($*OUT) tracks bytes written
my $before = $*OUT.tell;
print "";
ok $*OUT.tell >= $before, 'tell on $*OUT is monotonic';

# negative seek from the beginning is an error
my $fh2 = $path.open;
dies-ok { $fh2.seek(-5, SeekFromBeginning) }, 'negative seek from beginning dies';
$fh2.close;

# .t (is-tty) is a Bool and does not throw on a file handle
my $fh3 = $path.open;
ok $fh3.t ~~ Bool, '.t returns a Bool on a file handle';
nok $fh3.t, 'a regular file is not a TTY';
$fh3.close;

$path.unlink;
