use v6;
use lib 't/lib';
use Test;
use UsesNativeCallThenLibs;

plan 1;

# NativeCall's `sub EXPORT` hook hands out the `:native` candidate of
# `trait_mod:<is>`, a family the module's `use NativeCall` also imports. A
# module that uses NativeCall and then NativeLibs (whose own hook looks that
# candidate up) must still see it (#14140, DBIish::Pg).
is UsesNativeCallThenLibs::native-candidates(), 1,
    'a module sees the :native trait_mod candidate its use of NativeCall imported';
