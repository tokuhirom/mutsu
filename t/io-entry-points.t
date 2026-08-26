use v6;
use Test;

# The sub forms of the IO entry points must agree with their method forms about
# how they coerce their argument: `lines($path)` is `$path.lines`, and `open`
# accepts a named adverb written before the positional path.

plan 26;

my $dir = $*TMPDIR.add("mutsu-io-entry-points-{$*PID}");
$dir.mkdir;
LEAVE {
    for $dir.dir -> $f { $f.unlink }
    $dir.rmdir;
}

my $file = $dir.add("lines.txt");
$file.spurt("alpha\nbeta\ngamma\n");

# --- lines() in sub form ---------------------------------------------------

is-deeply lines($file).List, ("alpha", "beta", "gamma").List,
    'lines(IO::Path) reads the file, like $path.lines';
is-deeply lines($file).List, $file.lines.List,
    'lines(IO::Path) sub form agrees with the .lines method form';
is lines($file).^name, 'Seq', 'lines(IO::Path) returns a Seq';
nok lines($file).is-lazy, 'lines(IO::Path) is not lazy';
is-deeply lines($file, 2).List, ("alpha", "beta").List,
    'lines(IO::Path, $limit) truncates';

# A second call re-reads the file (no handle state is consumed).
is-deeply lines($file).List, ("alpha", "beta", "gamma").List,
    'lines(IO::Path) can be called twice';

# A plain Str positional is still split as a string, not opened as a path.
is-deeply lines("one\ntwo").List, ("one", "two").List,
    'lines(Str) splits the string itself';

# An IO::Handle positional reads through the handle, and consumes it.
{
    my $fh = $file.open;
    is-deeply lines($fh).List, ("alpha", "beta", "gamma").List,
        'lines(IO::Handle) reads through the handle';
    is-deeply lines($fh).List, ().List,
        'lines(IO::Handle) on an exhausted handle is empty';
    $fh.close;
}

# --- words() in sub form ---------------------------------------------------

is-deeply words($file).List, ("alpha", "beta", "gamma").List,
    'words(IO::Path) reads the file, like $path.words';
is-deeply words($file).List, $file.words.List,
    'words(IO::Path) sub form agrees with the .words method form';
is words($file).^name, 'Seq', 'words(IO::Path) returns a Seq';
is-deeply words($file, 2).List, ("alpha", "beta").List,
    'words(IO::Path, $limit) truncates';
is-deeply words("a b c").List, ("a", "b", "c").List,
    'words(Str) splits the string itself';
{
    my $fh = $file.open;
    is-deeply words($fh).List, ("alpha", "beta", "gamma").List,
        'words(IO::Handle) reads through the handle';
    $fh.close;
}

# --- open() with a named adverb before the positional path -----------------

my $out = $dir.add("open-adverb.txt");

{
    my $fh = open :w, $out.absolute;
    $fh.say: "written";
    $fh.close;
    is $out.slurp, "written\n", 'open(:w, $path) writes (adverb before path)';
}

{
    my $fh = open :r, $out.absolute;
    is $fh.get, "written", 'open(:r, $path) reads (adverb before path)';
    $fh.close;
}

{
    my $fh = open :a, $out.absolute;
    $fh.say: "appended";
    $fh.close;
    is $out.slurp, "written\nappended\n", 'open(:a, $path) appends (adverb before path)';
}

{
    my $fh = open :bin, $out.absolute;
    is-deeply $fh.read(3).list, (119, 114, 105),
        'open(:bin, $path) opens in binary mode (adverb before path)';
    $fh.close;
}

{
    my $fh = open :enc<utf8>, $out.absolute;
    is $fh.get, "written", 'open(:enc<utf8>, $path) honours the encoding (adverb before path)';
    $fh.close;
}

{
    # The path may be an IO::Path object, not just a Str.
    my $fh = open :r, $out;
    is $fh.get, "written", 'open(:r, IO::Path) accepts an IO::Path after the adverb';
    $fh.close;
}

{
    # Trailing-adverb spelling keeps working, and both spellings agree.
    my $fh = open $out.absolute, :r;
    is $fh.get, "written", 'open($path, :r) still works (adverb after path)';
    $fh.close;
}

{
    # Adverbs on both sides of the path.
    my $two = $dir.add("open-two-adverbs.txt");
    my $fh = open :w, $two.absolute, :nl-out("\n");
    $fh.print: "x";
    $fh.close;
    is $two.slurp, "x", 'open(:w, $path, :nl-out) mixes adverbs on both sides';
}

# --- slurp/spurt already agree; pin that they stay that way ----------------

is slurp($file), "alpha\nbeta\ngamma\n", 'slurp(IO::Path) reads the file';
is slurp($file.absolute), "alpha\nbeta\ngamma\n", 'slurp(Str) reads the file';

{
    my $sp = $dir.add("spurt.txt");
    spurt $sp, "spurted";
    is $sp.slurp, "spurted", 'spurt(IO::Path, $text) writes the file';
}

done-testing;
