# A block's call writes no `@_` it does not own

A block has an `@_` of its own only when it declares one or reads one (the
implicit `*@_`). mutsu's binders still published a fresh `@_` on every block
call. Inside `sub nested(*@_) { { { @_.join(',') }() }() }`, the middle block's
call left an empty `@_` in its frame. The innermost block's capture took that
one instead of the routine's, so `nested(1, 2)` answered an empty string where
rakudo prints `1,2` (#14024). `OUTER::OUTER::<@_>` showed the same thing.

The general binder (`FunctionBindingOptions::is_block`) and both light closure
binders (`call_owns_args_array`) now skip the write for a block whose body does
not read `@_` and whose signature does not declare it. Routines still always
get one. A block that owns `@_`, through `-> *@_` or a body that reads it, still
binds its own leftover or all positional arguments.
