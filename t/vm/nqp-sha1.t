use v6;
use Test;
use nqp;

# nqp::sha1($str) returns the SHA-1 digest of the string's UTF-8 encoding as
# 40 uppercase hex digits. Two components mutsu ships need it: the vendored
# zef (`Zef::Distribution.id`, plus the source-path computation in
# `Zef::CLI`'s `locate`) and bundled OpenSSL's `dll-resource()`.
#
# All expected digests below are the values rakudo's nqp::sha1 produces.

plan 12;

is nqp::sha1(""), 'DA39A3EE5E6B4B0D3255BFEF95601890AFD80709',
    'the empty string';
is nqp::sha1("abc"), 'A9993E364706816ABA3E25717850C26C9CD0D89D',
    'the NIST "abc" vector';
is nqp::sha1("hello"), 'AAF4C61DDCC5E8A2DABEDE0F3B482CD9AEA9434D',
    'a short string';
is nqp::sha1("The quick brown fox jumps over the lazy dog"),
    '2FD4E1C67A2D28FCED849EE1BB76E7391B93EB12',
    'a sentence';
is nqp::sha1("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"),
    '84983E441C3BD26EBAAE4AA1F95129E5E54670F1',
    'the NIST 448-bit vector';

# Padding edge cases: at 55 bytes the length still fits in the first block; at
# 56 and 64 it does not, so the padding spills into a second block.
is nqp::sha1("a" x 55), 'C1C8BBDC22796E28C0E15163D20899B65621D65A',
    '55 bytes (padding fits in one block)';
is nqp::sha1("a" x 56), 'C2DB330F6083854C99D4B5BFB6E8F29F201BE699',
    '56 bytes (padding spills into a second block)';
is nqp::sha1("a" x 64), '0098BA824B5C16427BD7A1122A5A442A25EC644D',
    'exactly one block';
is nqp::sha1("a" x 1000), '291E9A6C66994949B57BA5E650361E98FC36B1BA',
    'a multi-block message';

# Non-ASCII hashes the UTF-8 bytes, not the codepoints.
is nqp::sha1("日本語"), 'C12140A0FFB4E56481B4FE0A7A25040C2EAFA9CA',
    'a non-ASCII string hashes its UTF-8 encoding';
is nqp::sha1("\x[1D11E]x"), '116693A63839D657C9461C06193DA339DFFF4EF3',
    'including astral codepoints';

# The result is a plain Str, which is what callers concatenate and use as a
# path component (`$curi.prefix.child('sources').child($lib-sha1)`).
ok nqp::sha1("hello") ~~ Str && nqp::sha1("hello").chars == 40,
    'the digest is a 40-character Str';
