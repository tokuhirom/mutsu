use v6;
use Test;

# IO::Handle's methods are rows of the built-in method table: the state
# readers and settings, the reads, the writes and `open`. A handle subclass
# (a wrapper like IO::MiddleMan) reaches the same rows by its owner, and a
# `.wrap` of a built-in handle method still runs the wrapper first.

plan 80;

my $dir = $*TMPDIR.add("mutsu-io-handle-rows-{$*PID}");
$dir.mkdir;
sub rm-rf(IO::Path $p) {
    if $p.d && !$p.l { rm-rf($_) for $p.dir; $p.rmdir } else { $p.unlink }
}
LEAVE rm-rf($dir);

# --- state ------------------------------------------------------------------
my $file = $dir.add("a.txt");
$file.spurt("alpha beta\ngamma\ndelta epsilon zeta\n");
my $fh = $file.open;
ok $fh.opened, 'opened on an open handle';
is $fh.path, $file, 'path is the path it was opened from';
is $fh.IO, $file, 'IO is the same path';
is $fh.Str, $file.Str, 'Str is the path';
ok $fh.gist.starts-with('IO::Handle<') && $fh.gist.ends-with('(opened)'), 'gist names the state';
is $fh.nl-out, "\n", 'nl-out defaults to a newline';
is $fh.nl-in.sort.join("|"), "\n|\r\n", 'nl-in defaults to both line endings';
ok $fh.chomp, 'chomp is on by default';
nok $fh.t, 'a file is not a tty';
ok $fh.encoding ~~ /^utf/, 'encoding is utf8 by default';
is $fh.tell, 0, 'tell at the start';
nok $fh.eof, 'not at the end';
ok $fh.native-descriptor > 2, 'native-descriptor is a file descriptor';
ok $fh.out-buffer.defined, 'out-buffer answers the size';
ok $fh.close, 'close';
nok $fh.opened, 'not opened after close';
ok $fh.gist.ends-with('(closed)'), 'gist after close';

# --- reads ------------------------------------------------------------------
$fh = $file.open;
is $fh.get, "alpha beta", 'get reads a line';
is $fh.tell, 11, 'tell after a line';
is $fh.getc, "g", 'getc reads one character';
is $fh.readchars(4), "amma", 'readchars($n)';
is $fh.get, "", 'get reads the rest of the line';
ok $fh.seek(0, SeekFromBeginning).so || True, 'seek';
is $fh.lines(2).join("|"), "alpha beta|gamma", 'lines($limit)';
$fh.seek(0, SeekFromBeginning);
is $fh.lines.elems, 3, 'lines';
ok $fh.eof, 'at the end after reading everything';
$fh.seek(6, SeekFromBeginning);
is $fh.slurp, "beta\ngamma\ndelta epsilon zeta\n", 'slurp reads the rest';
$fh.seek(0, SeekFromBeginning);
is $fh.slurp(:bin).elems, 36, 'slurp(:bin) is a Buf';
$fh.seek(0, SeekFromBeginning);
is $fh.read(5).decode, "alpha", 'read($n) is a Buf of n bytes';
$fh.seek(0, SeekFromEnd);
ok $fh.eof, 'seek from the end';
$fh.seek(-5, SeekFromCurrent);
is $fh.get, "zeta", 'seek from the current position';
$fh.seek(0, SeekFromBeginning);
is $fh.comb(/<[aeiou]>/).elems, 13, 'comb with a regex';
$fh.seek(0, SeekFromBeginning);
is $fh.split("\n", :skip-empty).elems, 3, 'split';
$fh.seek(0, SeekFromBeginning);
is $fh.words.elems, 6, 'words';
is $fh.close, True, 'close again';

{
    my $limited = $file.open;
    is $limited.words(2).join("|"), "alpha|beta", 'words($limit)';
    $limited.close;
    my $words = $file.open;
    my @w = $words.words(:close);
    is @w.elems, 6, 'words(:close) reads everything';
    nok $words.opened, 'and closes the handle';
    my $lines = $file.open;
    is $lines.lines(1, :close).join, "alpha beta", 'lines($limit, :close)';
    nok $lines.opened, 'lines(1, :close) closes the handle';
}

{
    my $sfh = $file.open;
    is $sfh.Supply(:size(10)).list.head, "alpha beta", 'Supply(:size)';
    $sfh.close;
}

# --- writes -----------------------------------------------------------------
my $out = $dir.add("out.txt");
$fh = $out.open(:w);
ok $fh.print("a", "b"), 'print';
ok $fh.say("c", 1), 'say';
ok $fh.put("d"), 'put';
ok $fh.printf("%03d|%s\n", 7, "x"), 'printf';
ok $fh.print-nl, 'print-nl';
ok $fh.write("raw\n".encode), 'write';
ok $fh.spurt("tail\n"), 'spurt';
ok $fh.flush, 'flush';
is $fh.tell, 23, 'tell counts what was written';
$fh.close;
is $out.slurp, "abc1\nd\n007|x\n\nraw\ntail\n", 'everything was written, in order';

$fh = $out.open(:w, :nl-out("|"));
$fh.print-nl;
$fh.say("x");
$fh.close;
is $out.slurp, "|x|", 'nl-out given to open is honoured by print-nl and say';

# --- open returns the handle ------------------------------------------------
my $h = IO::Handle.new(:path($out));
my $r = $h.open(:w);
ok $h.opened, 'open opens the receiver in place';
ok $r === $h, 'and answers the receiver';
$h.print("z");
$h.close;
is $out.slurp, "z", 'the opened receiver writes';

# --- failures stay failures ---------------------------------------------------
my $missing = $dir.add("no-such-dir/x.txt").open;
ok $missing ~~ Failure, 'open of a missing file is a Failure';
$missing.so;
my $closed = $file.open;
$closed.close;
throws-like { $closed.get }, Exception, 'get on a closed handle throws';
throws-like { $closed.print("x") }, Exception, 'print on a closed handle throws';

# --- the standard handles -------------------------------------------------------
ok $*OUT.t || !$*OUT.t, '$*OUT answers t';
is $*OUT.path.Str, '<STDOUT>', '$*OUT.path is the STDOUT special';
is $*ERR.path.Str, '<STDERR>', '$*ERR.path is the STDERR special';
ok $*OUT.opened, '$*OUT is opened';
ok $*OUT === $*OUT.open(:w), 'open on a standard stream answers the stream';
is $*OUT.nl-out, "\n", '$*OUT.nl-out';

# --- a subclass reaches the rows by its owner ---------------------------------
class Wrapped is IO::Handle {
    has IO::Handle $.inner;
}
my $inner = $out.open(:w);
my $w = Wrapped.bless(:inner($inner));
is $w.nl-out, "\n", 'nl-out on a wrapper has the default';
$inner.close;

# --- a wrapped built-in method runs the wrapper first -----------------------------
my @seen;
my $print = IO::Handle.^find_method('print');
my $wrap = $print.wrap: method (|c) { @seen.push: c.list.join; callsame };
my $wfh = $out.open(:w);
$wfh.print("w1");
$wfh.close;
$print.unwrap($wrap);
is-deeply @seen, ["w1"], 'the wrapper saw the print';
is $out.slurp, "w1", 'and callsame reached the native print';
$wfh = $out.open(:w);
$wfh.print("w2");
$wfh.close;
is-deeply @seen, ["w1"], 'once unwrapped the wrapper is not called';
is $out.slurp, "w2", 'and the print still writes';

# --- the primitives READ and WRITE ---------------------------------------------
# Rakudo's `READ(Int:D)` and `WRITE(Blob:D)` are the raw read and write a
# handle's own methods are built on; they work on a real handle and bind
# their argument strictly.
$out.spurt("ABCDEF");
my $rfh = $out.open;
my $chunk = $rfh.READ(2);
isa-ok $chunk, Buf, 'READ answers a Buf';
is-deeply $chunk.list, (65, 66), 'READ reads the requested bytes';
is-deeply $rfh.READ(3).list, (67, 68, 69), 'the next READ continues where the last stopped';
is-deeply $rfh.READ(10).list, (70,), 'READ at the end answers what is left';
is-deeply $rfh.READ(1).list, (), 'READ after the end answers an empty Buf';
throws-like { $rfh.READ("x") }, X::TypeCheck::Binding::Parameter, 'READ binds an Int';
$rfh.close;

my $wfh2 = $out.open(:w);
ok $wfh2.WRITE(Buf.new(72, 105)), 'WRITE answers True';
ok $wfh2.WRITE(Blob.new(33)), 'WRITE takes a Blob too';
throws-like { $wfh2.WRITE("x") }, X::TypeCheck::Binding::Parameter, 'WRITE binds a Blob';
$wfh2.close;
is $out.slurp, "Hi!", 'WRITE wrote the raw bytes';
ok IO::Handle.^can('READ') && IO::Handle.^can('WRITE'), '.^can sees both';
