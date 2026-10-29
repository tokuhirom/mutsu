# `with %h<a><b>` no longer builds a cyclic cell chain

`with`/`given` over a nested element (`%n<a><tf>`) writes the topic back to its element. When the
intermediate element `%n<a>` was a shared binding cell (left there by a `%n{$k} = ...` rewrite of a
hash a sub captures), the write-back stored that cell back under its own key, inside itself. The
next read of `%n<a>` recursed through the cycle until the native stack overflowed (SIGSEGV;
LLM::Graph's `test-function` nodes). The write now goes THROUGH such a cell and leaves it in place.

Defence in depth: `Value::with_deref` walks a chain of cells iteratively and stops at 4096 cells
instead of recursing, so a cell reachable from itself returns the cell it reached rather than
aborting the process.
