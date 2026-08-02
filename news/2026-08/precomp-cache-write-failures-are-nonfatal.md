# Make precomp cache writes optional

Module loading now continues when the precompilation cache directory cannot be
created or written. The first cache failure in a process emits a warning, while
later failures stay quiet to avoid flooding stderr. Precomp unit tests also use
isolated temporary cache directories instead of depending on the user's cache.
