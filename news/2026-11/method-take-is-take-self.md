# `.take` is `take self`, and a user object's `.take` takes the short path

Rakudo's `Mu.take` is `method take { take self }`. mutsu's method row pushed
the invocant straight into the enclosing gather's buffer instead, so it
skipped what the statement does: outside a gather `$obj.take` did nothing
where Rakudo throws `X::ControlFlow`, a `CONTROL { when CX::Take { ... } }`
never saw it, and a lazily built gather taken this way was not reified at
the take. The row now runs the statement's own implementation
(`take_statement`), shared with the `Take` opcode.

The method also took the whole method cascade on an instance of a user class,
about 35k instructions per call. EEL walks its pattern trees with
`gather { take $node; for $node.children { .take for nodes($_) } }`, which
takes every node once per ancestor: `.take` was the most frequent fallback in
its suite (39,011 calls in `t/42-cuts.t`). A lane at the top of the two
method-call opcodes now answers `.take` on a user-class instance that declares
no `take` of its own (directly, from a role, or through `handles`).

Turning that tree walk into an array went from 2.8 ms to 1.2 ms for a
31-node tree (release build; rakudo about 1.5 ms).
