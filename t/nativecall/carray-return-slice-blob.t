use Test;
use NativeCall;
plan 7;

# Crypt::LibGcrypt: `gcry_md_read(--> CArray[uint8])` hands back C memory, and
# the module builds `Blob.new($carray[^$len])` from it.
sub strerror(int32 --> CArray[uint8]) is native {*}
my $r = strerror(2);
ok $r.defined, 'a CArray[uint8] return is a defined CArray over C memory';
is $r[0], 78, 'element 0 reads the C byte';
is $r[1], 111, 'element 1 reads the C byte';
is-deeply $r[^2].list, (78, 111), 'a slice reads the C bytes';
is-deeply Blob.new($r[^2]).list, (78, 111), 'Blob.new of a CArray slice takes the bytes';

my $c = CArray[uint8].new(5, 6, 7, 8);
is-deeply Blob.new($c[1, 2]).list, (6, 7), 'Blob.new of a managed CArray slice';
is-deeply buf8.new($c[^4]).list, (5, 6, 7, 8), 'buf8.new of a managed CArray slice';
