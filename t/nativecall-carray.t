use Test;

# NativeCall CArray[T] support: a `CArray[T]` is a contiguous C buffer that can
# be constructed, indexed, and passed to a native function as a `T*` (or a
# `char**` for CArray[Str]). Data the callee writes into the buffer is copied
# back into the Raku array (out-array semantics).

use NativeCall;

plan 21;

# --- Raku-side construction / indexing (numeric) ---
my $u = CArray[uint8].new(104, 105, 0);
isa-ok $u, CArray[uint8],            'CArray[uint8].new returns a CArray[uint8]';
is $u.elems, 3,                      'CArray[uint8] has the constructed elems';
is $u[0], 104,                       'CArray[uint8] positional read';
is $u[1], 105,                       'CArray[uint8] positional read (2)';

my $i = CArray[int32].new;
$i[0] = 1000;
$i[1] = 2000;
is $i.elems, 2,                      'empty CArray[int32].new grows on element assign';
is $i[1], 2000,                      'CArray[int32] element assign/read';

# --- Raku-side construction / indexing (Str) ---
my $s = CArray[Str].new('alpha', 'beta', 'gamma');
is $s.elems, 3,                      'CArray[Str] has the constructed elems';
is $s[2], 'gamma',                   'CArray[Str] positional read';
$s[1] = 'BETA';
is $s[1], 'BETA',                    'CArray[Str] element assign/read';

# --- strlen over a CArray[uint8] used as a NUL-terminated char* ---
sub c_strlen(CArray[uint8] $p) returns int64 is native('c') is symbol('strlen') { * }
is c_strlen(CArray[uint8].new(104, 105, 0)), 2, 'strlen(CArray[uint8]) counts to NUL';

# --- memcmp over two numeric CArrays (read-only, two buffers) ---
sub c_memcmp(CArray[uint8] $a, CArray[uint8] $b, size_t $n) returns int32
    is native('c') is symbol('memcmp') { * }
my $a = CArray[uint8].new(1, 2, 3);
my $b = CArray[uint8].new(1, 2, 3);
my $c = CArray[uint8].new(1, 2, 4);
is c_memcmp($a, $b, 3), 0,           'memcmp of equal uint8 buffers is 0';
ok c_memcmp($a, $c, 3) < 0,          'memcmp orders unequal uint8 buffers';

# --- int32 element type (memcmp over 2 int32 = 8 bytes) ---
sub c_memcmp32(CArray[int32] $a, CArray[int32] $b, size_t $n) returns int32
    is native('c') is symbol('memcmp') { * }
my $x = CArray[int32].new(1000, 2000);
my $y = CArray[int32].new(1000, 2000);
my $z = CArray[int32].new(1000, 2001);
is c_memcmp32($x, $y, 8), 0,         'memcmp of equal int32 buffers is 0';
ok c_memcmp32($x, $z, 8) != 0,       'memcmp of unequal int32 buffers is non-zero';

# --- memcpy fills the destination CArray (writeback / out-array) ---
sub c_memcpy(CArray[uint8] $dst, CArray[uint8] $src, size_t $n) returns Pointer
    is native('c') is symbol('memcpy') { * }
my $dst = CArray[uint8].new(0, 0, 0, 0);
my $src = CArray[uint8].new(9, 8, 7, 6);
c_memcpy($dst, $src, 4);
is $dst[0], 9,                       'memcpy wrote back element 0';
is $dst[3], 6,                       'memcpy wrote back element 3';
is-deeply (^4).map({ $dst[$_] }).List, (9, 8, 7, 6), 'memcpy filled the whole dst buffer';

# --- char** end-to-end: strsep(char **stringp, char *delim) returns the first
#     token as char*, exercising the CArray[Str] -> char** marshalling ---
sub c_strsep(CArray[Str] $sp, Str $delim) returns Str is native('c') is symbol('strsep') { * }
is c_strsep(CArray[Str].new('one,two,three'), ','), 'one',
    'strsep over a CArray[Str] char** returns the first token';

# --- num64 element type round-trips through a byte-wise memcmp ---
sub c_memcmp64(CArray[num64] $a, CArray[num64] $b, size_t $n) returns int32
    is native('c') is symbol('memcmp') { * }
my $p = CArray[num64].new(1.5e0, 2.5e0);
my $q = CArray[num64].new(1.5e0, 2.5e0);
is c_memcmp64($p, $q, 16), 0,        'memcmp of equal num64 buffers is 0';

# --- smartmatch against the parametric type ---
ok CArray[int32].new(1, 2) ~~ CArray[int32], 'CArray value smartmatches its parametric type';
nok CArray[int32].new(1, 2) ~~ CArray[num64], 'CArray value does not match a different element type';
