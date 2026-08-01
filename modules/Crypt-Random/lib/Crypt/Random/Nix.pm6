use v6;
use nqp;
use strict;

unit module Crypt::Random::Nix;



sub _crypt_random_bytes(uint32 $len) returns Buf is export {
    my $urandom := nqp::open('/dev/urandom', 'r');
    my $bytes   := Buf.new;

    nqp::readfh($urandom, $bytes, $len);
    nqp::closefh($urandom);

    die "Failed to read enough bytes from /dev/urandom" if $bytes.elems != $len;

    $bytes;
}
