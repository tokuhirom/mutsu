# A Seq's `push-all` no longer binds the outer call's argument names

`Seq.new($iter).categorize(&prefix:<->)` died with "Callable expected"
when the iterator's class wrote its own sigilless `push-all(\target)`.
The method call recorded its argument names before it reified the Seq
receiver. Reifying it ran `push-all`, whose binder then read those
names as its own. It took `target` to come from `&prefix:<->` and wrote
the buffer back over the operator, so the mapper was no longer callable.

User code that a Seq source runs (an iterator's methods, a deferred
`.map` block) now runs with the pending call-argument names parked, and
they are restored afterwards for the call they belong to (#17935).
