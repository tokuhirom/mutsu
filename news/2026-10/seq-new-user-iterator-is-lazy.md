# A Seq over a user iterator reports the iterator's `is-lazy`

In Rakudo, a Seq's `is-lazy` delegates to its iterator. mutsu's `Seq.new`
ignored a user `does Iterator` class's own `method is-lazy`. So `.is-lazy`
answered `False`, and `say Seq.new(Ones.new)` pulled an infinite iterator
forever instead of printing `(...)` (#10864).

The fix asks the iterator when a reader needs the answer: `.is-lazy`,
`.gist`, or `say` / `note`. `Seq.new` itself does not ask, so no user code
runs inside it. `Interpreter::resolve_seq_iterator_laziness` checks whether
the iterator's class declares an `is-lazy` method. If it does and the method
returns `True`, the body is marked lazy. From then on `.is-lazy` reports it,
and `.gist` renders the placeholder without pulling. Built-in iterators keep
their laziness from construction, as before.
