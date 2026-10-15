use v6;
use Test;
use NativeCall;

# Archive::Libarchive::Raw (ecosystem): a callback parameter declared as a
# `CArray[T]` or as a `repr('CPointer')` class receives an instance of that
# type, not a bare Pointer (rakudo marshals by the callback's signature).
plan 5;

class Handle is repr('CPointer') { }

sub qsort(CArray[uint8], size_t, size_t, &cmp (CArray[uint8], CArray[uint8] --> int32)) is native {*}
sub qsort2(CArray[uint8], size_t, size_t, &cmp (Handle, Handle --> int32)) is native is symbol('qsort') {*}

my $arr = CArray[uint8].new(9, 3, 7);
my @seen;
sub cmp(CArray[uint8] $a, CArray[uint8] $b --> int32) { @seen.push: "{$a[0]}/{$b[0]}"; $a[0] <=> $b[0] }
qsort($arr, 3, 1, &cmp);
is "$arr[0]$arr[1]$arr[2]", "379", 'CArray[uint8] callback parameters sort the array';
ok @seen.elems > 0, 'callback ran';
ok @seen.all ~~ /^\d\/\d$/, 'elements read through the CArray parameters';

my $kind;
sub cmp2(Handle $a, Handle $b --> int32) {
    $kind = $a.WHAT.^name;
    nativecast(CArray[uint8], $a)[0] <=> nativecast(CArray[uint8], $b)[0]
}
my $arr2 = CArray[uint8].new(5, 1, 4);
qsort2($arr2, 3, 1, &cmp2);
is "$arr2[0]$arr2[1]$arr2[2]", "145", 'a CPointer-repr class parameter sorts via nativecast';
is $kind, 'Handle', 'the parameter arrives as that class';
