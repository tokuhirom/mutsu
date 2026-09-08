use Test;

# Two routines may each declare an inner `sub` of the SAME name; each body must
# see its own. mutsu's registry is name-keyed (`Pkg::name`), and a routine scope
# restores a whole snapshot of it when the body ends -- so the key can hold
# another routine's same-named inner sub by the time this one runs again. The
# registrar's idempotent re-registration fast path used to accept that as
# "already installed" (it only tested that SOMETHING was registered under the
# name), leaving the other routine's definition live inside this body.
#
# `Digest::SHA2` is the real case: `sha256` and `sha512` each declare
# `rotr`/`Sigma0`/`Sigma1`/`sigma0`/`sigma1`, and after a `sha384` call (which
# runs `sha512`'s body) `sha256` silently computed with `sha512`'s 64-bit
# rotations -- a wrong digest, no error. See
# https://github.com/tokuhirom/mutsu/issues/7555.

use Digest::SHA2;
use HMAC;

plan 4;

sub run-in-block(&body) { body() }

run-in-block {
    my ($key, $msg) = Blob.new(1..25), Blob.new(0xcd xx 50);
    is hmac(:$key, :$msg, hash => &sha384, block-size => 128).list».fmt('%02x').join,
        '3e8a69b7783c25851933ab6290af6ca77a9981480850009cc5577c6e1f573b4e' ~
        '6801dd23c4a7d679ccf8a386c674cffb',
        'HMAC-SHA-384 matches RFC 4231 test case 4';
}

run-in-block {
    my ($key, $msg) = Blob.new(0x0c xx 20), "Test With Truncation";
    is hmac(:$key, :$msg, hash => &sha224, block-size => 64).subbuf(0, 16).list».fmt('%02x').join,
        '0e2aea68a90c8d37c988bcdb9fca6fa8',
        'HMAC-SHA-224 matches RFC 4231 test case 5';
    is hmac(:$key, :$msg, hash => &sha256, block-size => 64).subbuf(0, 16).list».fmt('%02x').join,
        'a3b6167473100ee06e0c796c2955552b',
        'HMAC-SHA-256 still uses its own rotr after a SHA-512 call';
    is hmac(:$key, :$msg, hash => &sha512, block-size => 128).subbuf(0, 16).list».fmt('%02x').join,
        '415fad6271580a531d4179bc891d87a6',
        'HMAC-SHA-512 is right in the same run';
}
