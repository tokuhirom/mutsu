use Test;

# VM-native .get line read on a File+UTF8 IO::Handle (③後段 PR-D read side).
# Reads the next record via the handle's record reader, resolved in the VM. Must
# behave identically to the interpreter's native fork. ArgFiles / Stdin /
# non-UTF8 handles fall through and are exercised here too.

plan 10;

my $path = $*TMPDIR.child("mutsu-io-get-{$*PID}.txt");
$path.spurt("line one\nline two\nlast\n");

# sequential .get with chomp (the default), Nil at EOF
{
    my $fh = $path.open;
    is $fh.get, 'line one', '.get reads the first line (chomped)';
    is $fh.get, 'line two', '.get reads the second line';
    is $fh.get, 'last', '.get reads the last line';
    nok $fh.get.defined, '.get returns Nil at EOF';
    $fh.close;
}

# :!chomp keeps the line terminator
{
    my $fh = $path.open(:!chomp);
    is $fh.get, "line one\n", '.get with :!chomp keeps the newline';
    $fh.close;
}

# a custom nl-in separator is honored
{
    my $p = $*TMPDIR.child("mutsu-io-get-nl-{$*PID}.txt");
    $p.spurt("aXbXc");
    my $fh = $p.open;
    $fh.nl-in = "X";
    is $fh.get, 'a', '.get honors a custom nl-in separator';
    is $fh.get, 'b', '.get continues with the custom separator';
    is $fh.get, 'c', '.get reads the trailing record without a separator';
    $fh.close;
    $p.unlink;
}

# a non-UTF8 handle still decodes (falls through to the interpreter)
{
    my $p = $*TMPDIR.child("mutsu-io-get-latin1-{$*PID}.txt");
    $p.spurt("abc\n");
    my $fh = $p.open;
    $fh.encoding('latin1');
    is $fh.get, 'abc', '.get on a latin1 handle decodes (fall through)';
    $fh.close;
    $p.unlink;
}

# .get on a closed handle dies
{
    my $fh = $path.open;
    $fh.close;
    dies-ok { $fh.get }, '.get on a closed handle dies';
}

$path.unlink;
