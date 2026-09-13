use Test;
use NativeCall::Types;

# NativeCall::Types is a separately loadable upstream module. mutsu provides
# its declarations through the same NativeCall prelude, so consumers that only
# need the type surface do not have to import the NativeCall provider itself.

plan 8;

ok Pointer.^name.ends-with('Pointer'),
    'NativeCall::Types makes Pointer visible';
ok CArray.^name.ends-with('CArray'),
    'NativeCall::Types makes CArray visible';
ok int32.^name eq 'int32',
    'NativeCall::Types makes int32 visible';
ok size_t.^name eq 'NativeCall::Types::size_t',
    'NativeCall::Types makes size_t visible';

my int32 $number = 42;
is $number, 42, 'int32 remains usable as a native scalar';
my size_t $size = 4096;
is $size, 4096, 'size_t remains usable as a native scalar';

isa-ok CArray[int32].new, CArray[int32],
    'CArray is usable after loading NativeCall::Types';
isa-ok Pointer.new(0), Pointer,
    'Pointer is usable after loading NativeCall::Types';
