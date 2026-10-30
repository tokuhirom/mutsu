# A worker's re-declared, captured lexical no longer overwrites the spawner's same-named variable

Red's `t/99-transaction-guard` ("do and start outside red-do restore the context") ended
with the caller's `my $guard` replaced by another `Red::Transaction::Guard` after
`await($guard.start(...))`. The worker runs `Guard!invoke`, which re-declares
`my $guard = self` and captures it in a closure; `box_captured_lexicals` lifted the
re-declaration mask and published the closure's cell with `set_shared_var`, which writes
through to the nearest lineage that has the name, the spawner's. When the mask shows the
name was re-declared in this thread and only an ancestor lineage holds it, the cell is now
declared in the worker's own lineage instead.
