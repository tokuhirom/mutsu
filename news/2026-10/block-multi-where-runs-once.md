# A block-declared multi called from a closure runs its `where` once

A `multi` declared inside a block and called from a closure (`(^5).map({ k($_) })`)
reaches its family through the code value, by name. That path resolved the winner,
which ran its `where` clause (or `subset` predicate), and then bound the winner
without the "already checked" signal the direct call path gives the binder, so the
predicate ran a second time on every call (#13183).

The by-name fallback now arms the same trusted-verdict signal for a multi winner
that `dispatch_func_call_inner` arms (#8697), and so does the path that dispatches
through a multi's captured candidates after its block exited. That second path
also built its `callsame`/`nextsame` chain by testing every losing candidate
against the arguments up front, which ran their `where` clauses too; the chain is
now filtered lazily as the walk reaches each candidate, as the by-name chain is.
