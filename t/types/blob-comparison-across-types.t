use v6;
use Test;

# Rakudo's `infix:<eq>`/`infix:<ne>` have a `(Blob:D, Blob:D)` candidate that
# compares the BYTES, whatever the two Blob types are: `"hi".encode` (a `utf8`)
# compares equal to a `Buf[uint8]`/`Blob[uint8]` holding the same bytes. Only
# when one side is NOT a Blob does the `.Str` coercion apply, and there `utf8`
# is the one Blob type that decodes (`"hi".encode eq "hi"` is True).
#
# This file is measured against real `raku` as well as mutsu: every assertion
# below is green under both.

plan 33;

my $utf8    = "hi".encode;
my $utf8b   = "hi".encode;
my $buf     = Buf[uint8].new(104, 105);
my $buf2    = Buf[uint8].new(104, 105);
my $blob    = Blob[uint8].new(104, 105);

# --- same type ------------------------------------------------------------
ok  $utf8 eq $utf8b, 'utf8 eq utf8 (same bytes)';
nok $utf8 ne $utf8b, 'utf8 ne utf8 (same bytes) is False';
ok  $buf  eq $buf2,  'Buf[uint8] eq Buf[uint8] (same bytes)';
nok $buf  ne $buf2,  'Buf[uint8] ne Buf[uint8] (same bytes) is False';

# --- across Blob types ----------------------------------------------------
ok  $utf8 eq $buf,   'utf8 eq Buf[uint8] (same bytes)';
ok  $buf  eq $utf8,  'Buf[uint8] eq utf8 (same bytes)';
nok $utf8 ne $buf,   'utf8 ne Buf[uint8] (same bytes) is False';
nok $buf  ne $utf8,  'Buf[uint8] ne utf8 (same bytes) is False';
ok  $utf8 eq $blob,  'utf8 eq Blob[uint8] (same bytes)';
ok  $blob eq $utf8,  'Blob[uint8] eq utf8 (same bytes)';
ok  $buf  eq $blob,  'Buf[uint8] eq Blob[uint8] (same bytes)';
nok $buf  ne $blob,  'Buf[uint8] ne Blob[uint8] (same bytes) is False';

# --- differing bytes ------------------------------------------------------
my $other = Buf[uint8].new(104, 106);       # "hj"
nok $utf8 eq $other, 'utf8 eq Buf[uint8] (different bytes) is False';
ok  $utf8 ne $other, 'utf8 ne Buf[uint8] (different bytes)';
nok $blob eq $other, 'Blob[uint8] eq Buf[uint8] (different bytes) is False';
ok  $blob ne $other, 'Blob[uint8] ne Buf[uint8] (different bytes)';

# --- differing lengths ----------------------------------------------------
my $short = Buf[uint8].new(104);            # "h"
nok $utf8 eq $short, 'utf8 eq shorter Buf[uint8] is False';
ok  $utf8 ne $short, 'utf8 ne shorter Buf[uint8]';
nok $short eq $utf8, 'shorter Buf[uint8] eq utf8 is False';
ok  $short ne $utf8, 'shorter Buf[uint8] ne utf8';

# --- empty buffers --------------------------------------------------------
my $empty-utf8 = "".encode;
my $empty-buf  = Buf[uint8].new();
my $empty-blob = Blob[uint8].new();
ok  $empty-utf8 eq $empty-buf,  'empty utf8 eq empty Buf[uint8]';
ok  $empty-buf  eq $empty-blob, 'empty Buf[uint8] eq empty Blob[uint8]';
nok $empty-utf8 ne $empty-buf,  'empty utf8 ne empty Buf[uint8] is False';
nok $empty-utf8 eq $utf8,       'empty utf8 eq non-empty utf8 is False';
ok  $empty-utf8 ne $buf,        'empty utf8 ne non-empty Buf[uint8]';

# --- utf8 against a plain Str (the one Blob type with a working .Str) -----
ok  $utf8 eq "hi",   'utf8 eq Str (decoded text)';
ok  "hi"  eq $utf8,  'Str eq utf8 (decoded text)';
nok $utf8 ne "hi",   'utf8 ne Str (decoded text) is False';
nok $utf8 eq "hj",   'utf8 eq different Str is False';
ok  $utf8 ne "hj",   'utf8 ne different Str';

# --- multi-byte content ---------------------------------------------------
my $uni      = "\c[SNOWMAN]".encode;
my $uni-buf  = Buf[uint8].new($uni.list);
ok  $uni eq $uni-buf, 'multi-byte utf8 eq Buf[uint8] with the same bytes';
ok  $uni eq "\c[SNOWMAN]", 'multi-byte utf8 eq Str (decoded text)';
nok $uni eq $buf,     'multi-byte utf8 eq unrelated Buf[uint8] is False';

# vim: ft=raku
