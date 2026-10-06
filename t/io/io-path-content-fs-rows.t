use v6;
use Test;

# IO::Path's content rows (slurp, lines, words, comb, open), its filesystem
# mutation rows (spurt, mkdir, rmdir, unlink, chmod, copy, rename, move,
# symlink, link) and the rest (child, resolve, dir, Numeric) are rows of the
# built-in method table. A user subclass reaches the same rows by its owner.

plan 63;

my $dir = $*TMPDIR.add("mutsu-io-path-fs-rows-{$*PID}");
$dir.mkdir;
sub rm-rf(IO::Path $p) {
    if $p.d && !$p.l { rm-rf($_) for $p.dir; $p.rmdir } else { $p.unlink }
}
LEAVE rm-rf($dir);

# --- spurt and the content reads -----------------------------------------------
my $file = $dir.add("lines.txt");
ok $file.spurt("alpha beta\ngamma\ndelta epsilon zeta\n"), 'spurt writes';
is $file.slurp, "alpha beta\ngamma\ndelta epsilon zeta\n", 'slurp reads it back';
is $file.slurp(:bin).elems, 36, 'slurp(:bin) is a Buf of the bytes';
is $file.lines.join("|"), "alpha beta|gamma|delta epsilon zeta", 'lines';
is $file.lines(2).join("|"), "alpha beta|gamma", 'lines($limit)';
is $file.lines(:!chomp).head, "alpha beta\n", 'lines(:!chomp) keeps the newline';
is $file.words.join("|"), "alpha|beta|gamma|delta|epsilon|zeta", 'words';
is $file.words(3).join("|"), "alpha|beta|gamma", 'words($limit)';
is $file.comb(/<[aeiou]>/).elems, 13, 'comb with a regex';
is $file.comb(3).head(2).join("|"), "alp|ha ", 'comb with a size';
is $file.comb.elems, 36, 'comb with no matcher splits into characters';
ok $file.spurt("tail\n", :append), 'spurt :append';
is $file.slurp.lines.tail, "tail", 'the appended text is there';
$file.spurt("one\n");
nok $file.spurt("two\n", :createonly) ~~ Bool, 'spurt :createonly does not overwrite';
is $file.slurp, "one\n", 'the file is unchanged';
my $bytes = $dir.add("bytes.bin");
$bytes.spurt(Buf.new(1, 2, 255));
is $bytes.slurp(:bin).list, (1, 2, 255), 'spurt a Buf';
$bytes.spurt("caf\x[e9]", :enc<latin1>);
is $bytes.slurp(:enc<latin1>), "caf\x[e9]", 'spurt and slurp with :enc';

# --- open -----------------------------------------------------------------------
my $fh = $file.open;
isa-ok $fh, IO::Handle, 'open is an IO::Handle';
is $fh.get, "one", 'read through the handle';
$fh.close;
my $out = $dir.add("out.txt").open(:w);
$out.say("written");
$out.close;
is $dir.add("out.txt").slurp, "written\n", 'open(:w)';
my $gone = $dir.add("no-such-dir/x.txt").open;
ok $gone ~~ Failure, 'open of an unreachable path is a Failure';
$gone.so;

# --- mkdir, rmdir, unlink -------------------------------------------------------
my $sub = $dir.add("sub");
is $sub.mkdir.Str, $sub.Str, 'mkdir answers the path';
ok $sub.d, 'the directory exists';
ok $sub.add("a/b").mkdir, 'mkdir makes missing parents';
$sub.add("a/b").rmdir;
$sub.add("a").rmdir;
ok $sub.rmdir, 'rmdir';
nok $sub.e, 'the directory is gone';
ok $dir.add("missing").unlink, 'unlink of a missing file is True';
my $victim = $dir.add("victim");
$victim.spurt("x");
ok $victim.unlink, 'unlink';
nok $victim.e, 'the file is gone';
my $udir = $dir.add("udir");
$udir.mkdir;
my $unlinked = $udir.unlink;
ok $unlinked ~~ Failure, 'unlink of a directory is a Failure';
$unlinked.so;
$udir.rmdir;

# --- chmod ----------------------------------------------------------------------
my $mode = $dir.add("mode.txt");
$mode.spurt("m");
ok $mode.chmod(0o640), 'chmod';
is $mode.mode.Int, 0o640, 'the new mode';
ok $mode.chmod(0o600), 'chmod again';
is $mode.mode.Int, 0o600, 'the mode changes back';

# --- copy, rename, move ---------------------------------------------------------
my $src = $dir.add("src.txt");
$src.spurt("payload");
my $copy = $dir.add("copy.txt");
ok $src.copy($copy), 'copy';
is $copy.slurp, "payload", 'the copy has the content';
my $copy-again = $src.copy($copy, :createonly);
ok $copy-again ~~ Failure, 'copy :createonly onto an existing file is a Failure';
$copy-again.so;
my $renamed = $dir.add("renamed.txt");
ok $copy.rename($renamed), 'rename';
nok $copy.e, 'the old name is gone';
is $renamed.slurp, "payload", 'the renamed file has the content';
my $moved = $dir.add("moved.txt");
ok $renamed.move($moved), 'move';
nok $renamed.e, 'the moved-from name is gone';
is $moved.slurp, "payload", 'the moved file has the content';

# --- symlink and link -----------------------------------------------------------
my $link = $dir.add("symlink");
ok $src.symlink($link), 'symlink';
ok $link.l, 'the symlink is a link';
is $link.slurp, "payload", 'it reads through';
my $hard = $dir.add("hardlink");
ok $src.link($hard), 'link';
is $hard.slurp, "payload", 'the hard link reads through';
throws-like { $src.link }, Exception, 'link without a name is an error';

# --- child, resolve, dir, Numeric -------------------------------------------------
is $dir.child("x").Str, $dir.add("x").Str, 'child joins';
is $dir.child("x", :secure).Str, $dir.add("x").Str, 'child :secure';
is $dir.add("./src.txt").resolve.Str, $src.resolve.Str, 'resolve folds .';
ok $dir.dir.map(*.basename).grep("src.txt").so, 'dir lists the directory entries';
is $dir.dir(:test(/ ^ 's' /)).map(*.basename).sort.join(","), "src.txt,symlink", 'dir(:test)';
is "42".IO.Numeric, 42, 'Numeric of a numeric basename';
is "a/3.5".IO.Numeric, 3.5, 'Numeric reads only the basename';
ok "abc".IO.Numeric ~~ Failure, 'Numeric of a non-numeric basename is a Failure';
is "7".IO.Int, 7, 'Int through the Cool coercion';

# --- a user subclass reaches the rows by its owner -------------------------------
class MyPath is IO::Path { }
my $m = MyPath.new($src.Str);
is $m.slurp, "payload", 'slurp on a subclass';
is $m.lines.head, "payload", 'lines on a subclass';
is MyPath.new($dir.add("sub2").Str).mkdir.^name, "MyPath", 'mkdir on a subclass answers the subclass';
$dir.add("sub2").rmdir;
is $m.child("x").^name, "MyPath", 'child on a subclass keeps the class';
is MyPath.new("12").Numeric, 12, 'Numeric on a subclass';
