use Test;

# IO::Path.lines / .words open a handle and return its deferred Seq, as
# Rakudo's `self.open(...).lines(:close)` does, so `.head(n)` reads only a
# prefix of the file (#9257). The handle is private: it closes at EOF, after a
# consuming `.head` / `.first`, and when a `for` loop that claimed it is left.

plan 27;

my $dir = $*TMPDIR.add("mutsu-io-path-lines-lazy-$*PID");
$dir.mkdir;
LEAVE { .unlink for $dir.dir; $dir.rmdir }

my $f = $dir.add("a.txt");
$f.spurt: "\x[FEFF]one two\r\nthree\n\nfour  five\nsix";

is-deeply $f.lines, ("one two", "three", "", "four  five", "six").Seq, 'lines: BOM stripped, CRLF chomped';
is-deeply $f.words, <one two three four five six>.Seq, 'words';
is-deeply $f.lines(:!chomp), ("one two\n", "three\n", "\n", "four  five\n", "six").Seq, ':!chomp';
is-deeply $f.lines(2), ("one two", "three").Seq, 'lines with a limit';
is-deeply $f.words(3), <one two three>.Seq, 'words with a limit';
is-deeply $f.lines(:nl-in<e>), ("on", " two\nthr", "", "\n\nfour  fiv", "\nsix").Seq, ':nl-in';
is-deeply $f.lines.head(2), ("one two", "three").Seq, 'lines.head';
is-deeply $f.words.head(2), <one two>.Seq, 'words.head';
is $f.lines.first, 'one two', 'lines.first';
is $f.lines[1], 'three', 'lines[1]';
is $f.lines.elems, 5, 'lines.elems';
is $f.lines.^name, 'Seq', 'lines is a Seq';
is-deeply lines($f), $f.lines, 'lines sub form';
is-deeply words($f), $f.words, 'words sub form';
is $f.lines.map(*.uc).join('|'), 'ONE TWO|THREE||FOUR  FIVE|SIX', 'lines.map';
is-deeply $f.lines.kv, (0, "one two", 1, "three", 2, "", 3, "four  five", 4, "six").Seq, 'lines.kv';

my @seen;
for $f.lines -> $l { @seen.push: $l }
is-deeply @seen, ["one two", "three", "", "four  five", "six"], 'for over lines';

my $s = $f.lines;
$s.head(1);
throws-like { $s.List }, X::Seq::Consumed, '.head consumes the Seq';

my $e = $dir.add("e.txt");
$e.spurt: "";
is-deeply $e.lines, ().Seq, 'lines of an empty file';
is-deeply $e.words, ().Seq, 'words of an empty file';

my $b = $dir.add("b.txt");
$b.spurt: Buf.new(0xEF, 0xBB, 0xBF);
is $b.lines.elems, 0, 'a BOM-only file has no lines';
is $b.words.elems, 0, 'a BOM-only file has no words';

my $u = $dir.add("u.txt");
$u.spurt: "h\xe9llo\nw\n", :enc<latin1>;
is-deeply $u.lines(:enc<latin1>), ("h\xe9llo", "w").Seq, ':enc is honoured';

# A prefix read of a large file reads only the prefix.
my $big = $dir.add("big.txt");
$big.spurt: "abc def\n" x 200_000;
is-deeply $big.lines.head(3), ("abc def" xx 3).Seq, 'head of a large file';
is-deeply $big.words.head(3), <abc def abc>.Seq, 'words.head of a large file';

# The private handles do not accumulate open file descriptors. Rakudo leaves
# an abandoned handle to its GC (and so fails this check until a GC runs);
# mutsu has no handle finalizer, so it must close them eagerly or exhaust
# the fd limit in a loop.
my $fd-dir = "/proc/$*PID/fd".IO;
if $fd-dir.d {
    my $before = $fd-dir.dir.elems;
    for ^200 {
        my @h = $big.lines.head(2);
        my $x = $f.lines[0];
        my $w = $big.words.first;
        for $big.lines { last }
    }
    ok $fd-dir.dir.elems - $before < 10, 'prefix reads close their handles';
}
else {
    skip 'no /proc/PID/fd', 1;
}

dies-ok { $dir.add("nope.txt").lines.List }, 'lines of a missing file dies';
