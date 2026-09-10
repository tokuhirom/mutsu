use Test;

# VM-native character reads on a File+UTF8 IO::Handle: getc / readchars
# (③後段 PR-D3 read side). Resolved in the VM via the shared
# IoHandleState::read_chars_native. Must behave identically to the interpreter's
# native fork. utf16 / non-UTF8 / Stdin fall through.

plan 10;

my $path = $*TMPDIR.child("mutsu-io-gc-{$*PID}.txt");
$path.spurt("héllo");   # h, é (2 UTF-8 bytes), l, l, o

# .getc reads one (possibly multibyte) character at a time
{
    my $fh = $path.open;
    is $fh.getc, 'h', '.getc reads the first character';
    is $fh.getc, 'é', '.getc reads a multibyte character';
    is $fh.getc, 'l', '.getc continues';
    $fh.close;
}

# .getc returns Nil at EOF
{
    my $fh = $path.open;
    $fh.readchars(100);
    nok $fh.getc.defined, '.getc returns Nil at EOF';
    $fh.close;
}

# .readchars(N) reads up to N characters
{
    my $fh = $path.open;
    is $fh.readchars(2), 'hé', '.readchars(2) reads two characters';
    is $fh.readchars(100), 'llo', '.readchars past EOF returns the rest';
    is $fh.readchars(5), '', '.readchars at EOF returns the empty string';
    $fh.close;
}

# .readchars with no argument reads to EOF
{
    my $fh = $path.open;
    is $fh.readchars, 'héllo', '.readchars with no count reads everything';
    $fh.close;
}

# a non-UTF8 handle reads via the interpreter (falls through)
{
    my $p = $*TMPDIR.child("mutsu-io-gc-latin1-{$*PID}.txt");
    $p.spurt("abc");
    my $fh = $p.open;
    $fh.encoding('latin1');
    is $fh.getc, 'a', '.getc on a latin1 handle (fall through)';
    is $fh.readchars(2), 'bc', '.readchars on a latin1 handle (fall through)';
    $fh.close;
    $p.unlink;
}

$path.unlink;
