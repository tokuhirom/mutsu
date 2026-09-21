use Test;
use NativeCall :types;

plan 3;

is Pointer.^name, 'NativeCall::Types::Pointer',
    'NativeCall :types imports Pointer';
is CArray.^name, 'NativeCall::Types::CArray',
    'NativeCall :types imports CArray';
is long.^name, 'NativeCall::Types::long',
    'NativeCall :types imports C-width integer types';
