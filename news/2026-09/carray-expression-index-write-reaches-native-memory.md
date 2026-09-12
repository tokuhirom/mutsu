# A CArray element write reaches native memory from any expression

Assigning into an index of a native CArray handle silently did nothing
unless the handle was first bound to a variable. Both index-assignment
paths recognized a CArray handle by looking its **variable name** up in
`env`:

- `exec_index_assign_generic_op` (`vm_var_assign_index_named.rs`), for a
  computed target like `get()[2] = 7`, never even reached that lookup —
  its target is already the evaluated stack value, with no name to look
  up, so it fell into the generic Raku-container path and wrote into a
  throwaway array.
- `builtin_index_assign_method_lvalue` (`builtins_multidim_assign.rs`), for
  an accessor-returned handle like `$b.c[1] = 5`, called the accessor to
  get the current value and then dispatched `ASSIGN-POS`/`AT-POS` on it as
  though it were a plain Associative/Positional object.

Both now check the already-evaluated target/accessor-result value
directly for the CArray shape (an `Instance` with an `address` attribute
and a `CArray[...]` class name) and write through
`native_carray_element_assign` when it matches, before falling through to
their generic paths.

This matters because a `has CArray[T] $.x` CStruct field, and an inline
`HAS T @.x[N] is CArray` member, both read back as a proper `CArray[T]`
handle, so `$s.x[1] = 5` is a shape users reasonably write directly.

See [#8031](https://github.com/tokuhirom/mutsu/issues/8031) and the
regression test `t/nativecall/cstruct-carray-expr-index-write.t`.
