use Test;

# Pin for VM-native `IO::Path.open` (ledger §D ③ — the io_handles capstone).
# `.open` is the one IO::Path filesystem method that allocates an `io_handles`
# entry (`&mut self`). The VM *owns* `io_handles` (a shared Arc<RwLock>), so it
# dispatches `.open` natively via `try_io_path_open` — the single shared
# `open_file_handle` impl the interpreter also uses. Once open is native, the
# whole open -> read/write -> close lifecycle runs without the catch-all bounce.
# Both literal (`"x".IO.open`, non-mut path) and variable (`$p.open` ->
# CallMethodMut, mut path) receivers are exercised.

plan 20;

my $dir = "tmp/native-io-open-$*PID";
mkdir $dir;
my $r = "$dir/read.txt";
spurt $r, "L1\nL2\nL3\n";

# --- read mode (default / :r) ---
my $fh = $r.IO.open;
ok $fh ~~ IO::Handle,      '.open returns an IO::Handle';
is $fh.lines.elems, 3,     '.open then .lines reads all lines';
$fh.close;

my $fh2 = $r.IO.open(:r);
is $fh2.get, 'L1',         '.open(:r).get reads the first line';
is $fh2.get, 'L2',         '.open(:r).get advances to the next line';
$fh2.close;

# slurp through a handle
my $fh3 = $r.IO.open;
is $fh3.slurp, "L1\nL2\nL3\n", '.open then .slurp reads the whole file';
$fh3.close;

# --- write mode (:w) truncates ---
my $w = "$dir/write.txt";
my $wfh = $w.IO.open(:w);
ok $wfh ~~ IO::Handle,     '.open(:w) returns a handle';
$wfh.print("hello\n");
$wfh.print("world\n");
$wfh.close;
is $w.IO.slurp, "hello\nworld\n", '.open(:w) + .print wrote the content';

# re-open :w truncates
my $wfh2 = $w.IO.open(:w);
$wfh2.say("fresh");
$wfh2.close;
is $w.IO.slurp, "fresh\n",  '.open(:w) truncates on re-open';

# --- append mode (:a) ---
my $afh = $w.IO.open(:a);
$afh.say("appended");
$afh.close;
is $w.IO.slurp, "fresh\nappended\n", '.open(:a) appends';

# --- exclusive mode (:x via :exclusive) fails on an existing file ---
my $x = "$dir/excl.txt";
my $xfh = $x.IO.open(:w, :exclusive);
ok $xfh ~~ IO::Handle,     '.open(:exclusive) succeeds when the file is new';
$xfh.close;
my $xfail = $x.IO.open(:w, :exclusive);
nok $xfail.defined,        '.open(:exclusive) on an existing file is an undefined Failure';

# --- error cases return a Failure (not a thrown exception) ---
nok "$dir/missing.txt".IO.open(:r).defined,
    '.open(:r) on a missing file is an undefined Failure';
nok $dir.IO.open.defined,
    '.open on a directory is an undefined Failure';

# --- bin mode yields a binary handle ---
my $b = "$dir/bin.dat";
spurt $b, Buf.new(0x41, 0x42, 0x43);
my $bfh = $b.IO.open(:bin);
my $buf = $bfh.read(3);
ok $buf ~~ Blob,           '.open(:bin).read returns a Blob';
is $buf.elems, 3,          '.open(:bin).read got 3 bytes';
is $buf[0], 0x41,          '.open(:bin).read first byte';
$bfh.close;

# --- variable receiver: mut path (CallMethodMut) ---
my $p = $r.IO;
my $vfh = $p.open;
is $vfh.lines.elems, 3,    'variable receiver .open + .lines';
$vfh.close;

# .opened reflects handle state
my $ofh = $r.IO.open;
ok $ofh.opened,            '.opened is True for an open handle';
$ofh.close;
nok $ofh.opened,           '.opened is False after .close';

# cleanup
unlink $r; unlink $w; unlink $x; unlink $b;
rmdir $dir;
ok True, 'cleanup ran';
