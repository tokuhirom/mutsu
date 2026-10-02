# Class-body writes reach lexicals that the class's methods capture

A statement in a class body that writes a lexical of the enclosing scope, such
as `BEGIN { $cell = 5 }` or a bare `$x = 11` in a class declared inside a
routine, used to lose the write when a method of the same class read that
lexical. Both the method and the enclosing scope then saw the old value.
`my $cell; class A { BEGIN { $cell = 5 }; method n { $cell } }; say A.n`
printed `(Any)`, where rakudo prints `5`.

The class-body chunks write the outer variable into the env by name and queue a
caller-var writeback for the owning frame. Class registration then boxed each
lexical that a method captures into a shared cell, and it took the snapshot
before that writeback reached the slot, so the cell kept the stale value.
Registration now claims the pending writeback into the declaring frame's slots
before the method-capture pass boxes them (#10751).

The earlier claim exposed a second bug. A `my` that is the last statement of a
nested block in a class body (`class A { { my $p = 7 } }`) has no local slot. It
was stored under the package-qualified name `A::p`, and the free-variable
analysis read that store as a write to the enclosing scope's `$p`, so the outer
variable became 7. That declaration now stores under its bare name and is
recorded as the block's own binding, as an expression-position `my` already
was.
