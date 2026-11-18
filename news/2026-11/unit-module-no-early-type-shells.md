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

Two changes fix it (#18092):

- a `unit module`/`unit package` declarator without traits (adverbs such as
  `:ver<...>` included) now counts as a declaration, like `unit class`;
- a unit's shells register right after its last leading `use`, not at its very
  head: they still precede every statement that runs user code, but no longer
  the imports a role body's signatures and traits need. NativeHelpers'
  `NativeHelpers::CStruct`, whose `our $debug = False;` before the role makes
  the shell legitimate, now loads without the warning.
