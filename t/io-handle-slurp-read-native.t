use Test;

# VM-native bulk reads on a File+UTF8 IO::Handle: slurp (Str) and read (Buf)
# (③後段 PR-D2 read side). Resolved in the VM via the shared IoHandleState
# readers. Must behave identically to the interpreter's native fork. :bin /
# non-UTF8 slurp and ArgFiles/Stdin fall through.

plan 10;

my $path = $*TMPDIR.child("mutsu-io-sr-{$*PID}.txt");
$path.spurt("hello\nworld\n");   # 12 bytes

# .slurp returns the whole file as a Str
{
    my $fh = $path.open;
    is $fh.slurp, "hello\nworld\n", '.slurp reads the whole file';
    $fh.close;
}

# .slurp reads from the current position (after a partial read)
{
    my $fh = $path.open;
    $fh.get;                       # consumes "hello"
    is $fh.slurp, "world\n", '.slurp reads from the current position';
    $fh.close;
}

# .slurp on an already-exhausted handle is the empty string
{
    my $fh = $path.open;
    $fh.slurp;
    is $fh.slurp, "", '.slurp at EOF is the empty string';
    $fh.close;
}

# .read returns up to N bytes as a Buf
{
    my $fh = $path.open;
    my $buf = $fh.read(5);
    isa-ok $buf, Blob, '.read returns a Blob';
    is $buf.decode, "hello", '.read(5) reads five bytes';
    is $fh.read(100).elems, 7, '.read past EOF returns the remaining bytes';
    is $fh.read(100).elems, 0, '.read at EOF returns an empty Buf';
    $fh.close;
}

# .slurp(:bin) returns a Buf (falls through to the interpreter)
{
    my $fh = $path.open;
    is $fh.slurp(:bin).elems, 12, '.slurp(:bin) returns the raw bytes';
    $fh.close;
}

# a non-UTF8 handle slurps via decode (falls through)
{
    my $p = $*TMPDIR.child("mutsu-io-sr-latin1-{$*PID}.txt");
    $p.spurt("abc");
    my $fh = $p.open;
    $fh.encoding('latin1');
    is $fh.slurp, "abc", '.slurp on a latin1 handle decodes (fall through)';
    $fh.close;
    $p.unlink;
}

# .slurp on a closed handle dies
{
    my $fh = $path.open;
    $fh.close;
    dies-ok { $fh.slurp }, '.slurp on a closed handle dies';
}

$path.unlink;
