use Test;

# VM-native raw byte output on a File IO::Handle: write / spurt (③ native IO
# PR-D Tier-2c). Both write straight to the file (raw, bypassing :out-buffer and
# encoding), so they resolve in the VM via the shared
# IoHandleState::native_write_bytes_file. Must behave identically to the
# interpreter's native fork.

plan 8;

my $path = $*TMPDIR.child("mutsu-io-t2c-{$*PID}.bin");

# write a Blob
{
    my $fh = $path.open(:w);
    ok $fh.write(Buf.new(72, 73, 33)) === True, 'write(Blob) returns True';
    $fh.close;
    is-deeply $path.slurp(:bin).list, (72, 73, 33), 'write(Blob) writes the raw bytes';
}

# write multiple Blob args concatenates
{
    my $fh = $path.open(:w);
    $fh.write(Buf.new(1, 2));
    $fh.write(Buf.new(3, 4));
    $fh.close;
    is-deeply $path.slurp(:bin).list, (1, 2, 3, 4), 'multiple write() calls concatenate';
}

# spurt a Buf
{
    my $fh = $path.open(:w);
    ok $fh.spurt(Buf.new(10, 20, 30)) === True, 'spurt(Buf) returns True';
    $fh.close;
    is-deeply $path.slurp(:bin).list, (10, 20, 30), 'spurt(Buf) writes the raw bytes';
}

# spurt a Str (UTF-8) — including multibyte
{
    my $fh = $path.open(:w);
    $fh.spurt("café");
    $fh.close;
    is $path.slurp, "café", 'spurt(Str) writes UTF-8 bytes';
}

# spurt on a non-UTF8 handle still encodes (falls through)
{
    my $lp = $*TMPDIR.child("mutsu-io-t2c-latin1-{$*PID}.txt");
    my $fh = $lp.open(:w);
    $fh.encoding('latin1');
    $fh.spurt("é");
    $fh.close;
    is $lp.slurp(:bin).elems, 1, 'spurt(Str) on latin1 encodes (fall through)';
    $lp.unlink;
}

# writing to a closed handle dies
{
    my $fh = $path.open(:w);
    $fh.close;
    dies-ok { $fh.write(Buf.new(1)) }, 'write on a closed handle dies';
}

$path.unlink;
