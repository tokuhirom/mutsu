# EVAL'd units accept the names a `package EXPORT::<tag>` stash exports

A module that fills its import set through `package EXPORT::DEFAULT { our $answer; our &f }`
exports names no source scan can list, like a `sub EXPORT` hook. The CHECK-time
scans of an EVAL'd unit now treat such a module as an import hook: its variables
and routines are not rejected as undeclared (#16356). A tree (`EVAL($ast)`) and a
string unit behave the same. Tree units also lower under the revision of their
own `use v6.x`, so `my Int:D $x .= new: 42` follows 6.c in a v6.c tree.
