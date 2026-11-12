# Multi-method `where` clauses run only when the dispatch reaches them

mutsu matched every candidate of a `multi method` in full — `where` clauses
included — and then ranked the ones that bound. Rakudo ranks the candidates
on their nominal signatures and trial-binds a constrained candidate only when
its walk reaches it: the first constrained candidate that binds wins, and the
clauses of candidates ranked below it never run. A deferral (`callsame`,
`nextsame`) checks the following candidates when it resumes, inside the
callee, so a dynamic variable the callee set is visible to them.

Now a `multi` candidate with a `where` clause (or a literal parameter, which
rakudo checks in the same trial bind) is first matched on the rest of its
signature and ranked as if it bound; its full signature is checked only when
the ranking picks it, and a failure makes it a rival and re-runs the ranking.
A method deferral frame whose candidates carry `where` clauses is now built
lazily, like any other, when a deferral builtin first needs it — and only the
innermost pending frame is built, instead of every pending frame on the stack.

Besides matching rakudo's observable order of `where` side effects, this took
Red's AST optimizer (`multi method optimize(... where compare($left, .not))`)
from evaluating every candidate's comparison on every call and every
`nextsame`. Red's migration-verifier grep went from 6.8 s to 6.4 s per
evaluation on top of the call-site work; the rest is the per-call cost of
`ValueObjAt.new`, user `WHICH` methods and hyper method calls (#15737).
