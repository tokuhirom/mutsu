# A nested subscript argument with a computed index binds its container

`sub rw($x is rw) { $x = 9 }; rw(%h{@k[$i++]}<z>)` used to die with "expects a
writable container ... but got '(Any)'" when `%h{@k[0]}` did not exist yet
([#10044](https://github.com/tokuhirom/mutsu/issues/10044)). ADR-0059 Slice 3 compiles a
nested subscript argument of a named routine in `return-rw` container mode, whose lazy
autovivify ops leave a missing level as a deferred `HashEntryRef` token — but only when
every level's index was a literal or a plain variable. That static whitelist existed
because a level whose index turns out to be a slice or a `WhateverCode` at run time
(Base64's `slip(@alpha[$res[*]][0..*-1])`) was walked as if it named one location.

The whitelist is gone. Instead `IndexAutovivifyLazy` / `IndexAutovivifyLazyTerminal`
decline to the ordinary read whenever the run-time index selects several elements (a
list, range, `Seq`, `Junction`, `*` or unresolved `WhateverCode`) and the target is not
an `Array` (whose own arms already settle a list index, including the bound-slice cell
promotion). So `%g<a c>[1]`, `%g{'a'|'c'}<b>` and `%e<nope>{<a b>}` read exactly as
rakudo reads them, and any single-element computed index gets the deferred location:
the index expression still runs once, and the whole path is created on the first write.

Widening the path exposed a latent bug in the same code: the gate branched with
`JumpIfTrue`, which only peeks its condition, so the gate's `Bool` was left on the
stack under the argument. In a sub body the stray value was harmless, but a block whose
value is the call (`.map({ slip(@a[1][0]) })`) answered `False`. The gate is now negated
and branched on the popping `JumpIfFalse`.
