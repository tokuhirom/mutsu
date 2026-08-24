# `xx` thunks inherit the current topic

The re-evaluation thunk used by list repetition now has bare-block semantics,
so it closes over the current `$_` instead of resetting the topic like a
routine call. This fixes repeated topic-dependent calls inside deferred
closure sequences and lets the Rosetta Code evolutionary algorithm run to its
target string.
