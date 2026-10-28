use v6;
use lib 't/lib';
use Test;
use ExportHookFirstImporter;
use ExportHookLateNativeUser;

plan 2;

# NativeCall's `is native` is a `trait_mod:<is>` candidate its `sub EXPORT`
# supplies. A routine of a later importer applies the trait through what ITS
# unit imported, not through the env of the script that loaded an earlier
# importer first (#15103: NativeHelpers::Blob's `memcpy` under Archive::Libarchive).
my $src = Buf.new(1, 2, 3, 4);
my $copy = copy-bytes(pointer-to-bytes($src), :elems(4));
isa-ok $copy, Buf, 'a nested `my sub ... is native {*}` ran the native routine';
is $copy.list, (1, 2, 3, 4), 'the native memcpy copied the bytes';
