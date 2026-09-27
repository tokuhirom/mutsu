# Thread closures keep their own `@`/`%` binding across later calls

A `start` block, or a `.then` callback on a pending promise, that captured an
array or hash of the routine that created it used to read whatever a *later*
call of the same routine bound to that name: `sub h($n) { my @p = ^$n; start {
sleep 0.2; @p.elems } }; say await h(1), h(2), h(3)` printed `(3 3 3)` instead
of `(1 2 3)`. The cross-thread name lane holds one entry per name per spawn
lineage, and the second call's `my @p` overwrote the entry the first call's
thread was still using.

Now, when a lineage is about to replace or clear such an entry, it moves the
old binding (and its atomic push/element lane) into a small box and redirects
every live child that captured it there (ADR-0129). Children of the same
binding share the box, so they still see each other's writes.

A second, related hole caused the Tinky 0.1.5 `t/060-callbacks.t` hang: in
`my @promises = helper(0), helper(1), helper(2)`, each callee's own
`my @promises` ended the caller's declaration window early, so the caller's
store overwrote the entry the last callee's `.then` callback was reading, and
that callback waited on its own promise. The store that completes a
declaration now re-masks the name, so the binding is published at the next
spawn like any other declaration. Closes #9723.
