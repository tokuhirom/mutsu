use v6;
use nqp;
use Test;

# nqp::open / nqp::readfh / nqp::closefh -- the low-level file-handle ops
# Crypt::Random's Nix backend uses to read /dev/urandom.

plan 6;

# Read from a real file with known contents.
my $path = 'tmp/nqp-file-ops-fixture.bin';
spurt $path, Buf.new(1, 2, 3, 4, 5, 6, 7, 8);

my $fh = nqp::open($path, 'r');
my $buf = Buf.new;
nqp::readfh($fh, $buf, 5);
is $buf.elems, 5, 'readfh reads exactly the requested number of bytes';
is-deeply $buf.list, (1, 2, 3, 4, 5), 'readfh returns the leading bytes';

# readfh REPLACES the buffer contents (it does not append).
nqp::readfh($fh, $buf, 5);
is-deeply $buf.list, (6, 7, 8), 'a short read at EOF replaces the buffer';
nqp::closefh($fh);
unlink $path;

# /dev/urandom is the Crypt::Random shape: a fresh buffer per read.
my $ur = nqp::open('/dev/urandom', 'r');
my $bytes = Buf.new;
nqp::readfh($ur, $bytes, 16);
is $bytes.elems, 16, 'readfh reads 16 bytes from /dev/urandom';
my $bytes2 = Buf.new;
nqp::readfh($ur, $bytes2, 16);
nok $bytes eqv $bytes2, 'two urandom reads differ';
nqp::closefh($ur);

dies-ok { nqp::open('/nonexistent-dir-xyz/nope', 'r') },
    'opening a missing file dies';
