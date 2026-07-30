use v6;
use Test;
use NativeCall;

# mutsu implements NativeCall inside the VM, so `use NativeCall` loads no Raku
# module — but the module's export list is a real, introspectable surface.
# `NativeLibs` copies the whole `NativeCall::EXPORT::ALL` stash into its own
# `UNIT::EXPORT` so that its users get NativeCall transitively; with an empty
# stash that re-export silently did nothing.

plan 8;

ok ::('NativeCall') !~~ Failure, "::('NativeCall') resolves to the package";

my \exports = ::('NativeCall::EXPORT::ALL');
ok exports !~~ Failure, "::('NativeCall::EXPORT::ALL') resolves";

# A sigilless binding to a package names that package: the stash subscript must
# follow the *binding*, not look for a package literally called "exports".
ok exports::{'&nativecast'}:exists, 'the routine exports are listed';
ok exports::{'&trait_mod:<is>'}:exists, 'the `is native` trait is listed';
ok exports::{'Pointer'}:exists, 'the type exports are listed';
nok exports::{'&no-such-export'}:exists, 'an unexported name is absent';

my @want = <&trait_mod:<is> &nativecast &nativesizeof &cglobal &explicitly-manage
            &refresh &guess_library_name
            Pointer OpaquePointer CArray void bool long longlong ulong ulonglong size_t>;
my @missing = @want.grep({ !(exports::{$_}:exists) });
is @missing.join(' '), '', 'the whole documented export set is present';

# A user module's own stash gains an EXPORT member the same way.
module Exporter { our sub exported() is export { } }
ok Exporter::.keys.sort.join(' ') eq '&exported EXPORT',
   "a module that exports anything has an EXPORT stash member";

# vim: expandtab shiftwidth=4
