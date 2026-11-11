# A stubbed type's definition no longer jumps ahead of a versioned parent

When a compilation unit stubs a type (`grammar CSS::Grammar::Core {...}`) and
gives it its real definition later, the compiler moves that definition up next
to the stub, unless another type declaration sits in between. A versioned or
authored declaration (`grammar CSS::Grammar:ver<0.5.0> { ... }`) did not count
as one, because it arrives wrapped together with its `:ver`/`:auth` metadata
call. So in CSS::Grammar the child `CSS::Grammar::Core is CSS::Grammar` was
registered before its parent existed, and loading the module died with
"'CSS::Grammar::Core' cannot inherit from 'CSS::Grammar' because it is
unknown".

Classes had the same gap all along; grammars joined it when they started
keeping their name adverbs. The whole CSS family (CSS::Grammar, CSS::Writer,
CSS::Module, CSS::Properties, ... -- 25 dependents) went from green to
`blocked_load` in the 2026-11-11 sweep. CSS::Grammar's six test files now
match rakudo again, and its dependents load.
