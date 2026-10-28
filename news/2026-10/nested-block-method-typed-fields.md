# Nested-block method hoisting uses typed fields, not a marker trait

The `__nested_block_method` pseudo-trait is gone. A method hoisted out of a nested block of a
package body carries `MethodDecl::nested_capture_index`, and the hoisted copy of a nested
`proto method` carries `ProtoDecl::is_hoisted_copy`. `hoist`, `unhoist` and
`CompiledMethodDecl::from_stmt` read the fields instead of scanning `custom_traits`.
