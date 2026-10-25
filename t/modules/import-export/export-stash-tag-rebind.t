use lib $?FILE.IO.parent(3).add('lib');
use Test;
use ReexportsNativeCallTags;
use ReexportAndUseNativeCall;

# Binding `UNIT::EXPORT::{$tag}` to another module's `EXPORT::{$tag}` makes the
# loading module export everything that module exports under the tag. This is
# how NativeLibs re-exports NativeCall; every Red test file reaches `size_t` and
# `CArray` through it (via DB::Pg).

plan 6;

is size_t.^name, 'NativeCall::Types::size_t', 'a re-exported constant is imported';
is CArray[int32].^name, 'NativeCall::Types::CArray[int32]', 'a re-exported type is parameterizable';
is nativesizeof(int32), 4, 'a re-exported routine is imported';
ok &trait_mod:<is>.defined, 'a re-exported trait is imported';

# A module that both re-imports the constants through the re-exporting module
# and `use`s NativeCall itself keeps them in its own routines (the dynamic
# import must not clear the mainline pad binding; they read `Nil` before).
is pointer-name(), 'NativeCall::Types::Pointer', 'a routine of a module importing a constant twice sees it';
is size-name(), 'NativeCall::Types::size_t', 'so does the second constant';
