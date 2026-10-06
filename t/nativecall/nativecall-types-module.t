use Test;
use NativeCall::Types;

# NativeCall::Types is a separately loadable upstream module. Loading it alone
# declares the type surface under its qualified names; the short spellings
# (`Pointer`, `CArray`, `size_t`) are NativeCall's own exports, so a unit that
# only loads this module does not get them. Every expectation below was
# verified against Rakudo.

plan 10;

is NativeCall::Types::Pointer.^name, 'NativeCall::Types::Pointer',
    'NativeCall::Types makes Pointer visible by its qualified name';
is NativeCall::Types::CArray.^name, 'NativeCall::Types::CArray',
    'NativeCall::Types makes CArray visible by its qualified name';
is int32.^name, 'int32',
    'int32 is a core native type';
is NativeCall::Types::size_t.^name, 'NativeCall::Types::size_t',
    'NativeCall::Types makes size_t visible by its qualified name';

my int32 $number = 42;
is $number, 42, 'int32 remains usable as a native scalar';
my NativeCall::Types::size_t $size = 4096;
is $size, 4096, 'size_t remains usable as a native scalar';

isa-ok NativeCall::Types::CArray[int32].new, NativeCall::Types::CArray[int32],
    'CArray is usable after loading NativeCall::Types';
isa-ok NativeCall::Types::Pointer.new(0), NativeCall::Types::Pointer,
    'Pointer is usable after loading NativeCall::Types';

throws-like { EVAL 'Pointer' }, X::Undeclared::Symbols,
    'the short name Pointer is not exported by NativeCall::Types';
throws-like { EVAL 'CArray' }, X::Undeclared::Symbols,
    'nor is CArray';
