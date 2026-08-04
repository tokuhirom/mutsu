use Test;

# Digest is bundled (BATTERIES.md §7, docs/batteries/digest.md), so all six of
# its modules must load and work with a plain `use` -- no `-I`, no install.
# This pins the zero-config resolution and one published vector per algorithm;
# the exhaustive behaviour check is the release-time gate that runs the full
# upstream suite (scripts/battery-testsuite.sh).

plan 7;

use Digest::MD5;
use Digest::SHA1;
use Digest::SHA2;
use Digest::SHA3;
use Digest::RIPEMD;
use HMAC;

sub hex(Blob $b) { $b.list».fmt("%02x").join }

is hex(md5("abc")), '900150983cd24fb0d6963f7d28e17f72',
    'md5 matches the RFC 1321 vector';
is hex(sha1("abc")), 'a9993e364706816aba3e25717850c26c9cd0d89d',
    'sha1 matches the FIPS 180-1 vector';
is hex(sha256("abc")),
    'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad',
    'sha256 matches the FIPS 180-2 vector';
is hex(sha512("abc")).substr(0, 32), 'ddaf35a193617abacc417349ae204131',
    'sha512 matches the FIPS 180-2 vector';
is hex(sha3_256("abc")),
    '3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532',
    'sha3_256 matches the FIPS 202 vector';
is hex(rmd160("abc")), '8eb208f7e05d987a9b044a8e98c6b087f15a0bfc',
    'rmd160 matches the RIPEMD-160 reference vector';
is hex(hmac(key => "Jefe", msg => "what do ya want for nothing?",
            hash => &sha1, block-size => 64)),
    'effcdf6ae5eb2fa2d27416d5f184df9c259a7c79',
    'hmac-sha1 matches the RFC 2202 vector';
