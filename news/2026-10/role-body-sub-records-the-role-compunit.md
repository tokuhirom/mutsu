# A role body's `sub` belongs to the role's compunit wherever it is composed

A role's body runs again at each composition, from the composing scope.
mutsu already switched `?FILE` and the current compunit to the role's file
for that run, but a `sub` declared in the body recorded its source file
through `executing_source_file_for_module_load()`, which prefers the
innermost routine frame's file. When a class was composed from an `EVAL`
inside a sub, that frame was the caller's sub. The role's `sub det` was
then filed under the caller's compunit and could not see the routines the
role's own module imported:

```raku
# lib/EM.rakumod
use NativeCall;
role EM {
    sub det { my $i = CArray[uint16].new(0x1234); nativecast(CArray[uint8], $i)[0] }
    my constant H = det();
    method h { H }
}
# main.raku
sub f { EVAL q{use EM; class A does EM { }; A.new.h} }
say f();   # was "Unknown function: nativecast", now 52
```

`enter_source_file` now also marks the role body as that compunit's mainline
code (`module_loading_unit_stack`), as a module load does, so declarations
in the body record the role's file however deep the composer's call stack
is (#12895). Native::Packing's suite now passes all six files, including
`t/00-readme.t`, which evaluates its README snippets inside `lives-ok` blocks.
