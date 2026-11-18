# A role after a module's `use` is registered after the import

mutsu registers a type declared after run-time code early, as a "shell" at the
head of its scope, so the code before it can already reach it. A `unit
module X;` declarator was counted as such code, so in every module every role
(and class) after it was shelled ahead of the module's own `use` statements.
A role shell keeps its whole body, so a `sub calloc(size_t, size_t -->
Pointer) is native` in a role body was registered, and its trait applied,
before `use NativeCall` had imported `size_t`: NativeHelpers::Blob printed
NativeCall's "Not an accepted NativeCall type" warning for `size_t` whenever
another module had loaded NativeCall first.

A `unit module`/`unit package` declarator without traits now counts as a
declaration, like `unit class`, so the types after it register in source
order (#18092).
