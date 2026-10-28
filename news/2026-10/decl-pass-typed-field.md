# Which registration a declaration plan is for is a typed field, not marker traits

The compiler used to tell the registration code which pass a declaration plan belongs to by
appending argument-less marker traits (`__hoisted`, `__hoisted_nested`, `__lexical_hoist`,
`__nested_scope`) to a cloned statement's `custom_traits`, and about twenty places scanned the
trait list for the strings. The answer is now `ast::DeclPass`, a small `Copy` struct carried by the
sub, proto, class and role declaration plans and passed to `register_sub_decl*`.

Deleted: every marker push in the compiler (the cloned "marked" statements in `hoist_one_sub_decl`,
`nested_our_subs`, `emit_type_decl_shell`, the class-body hoist copy and both in-sequence
`SubDecl`/`ProtoDecl` lowerings), the `NESTED_SCOPE_DECL_MARKER` constant, all `custom_traits`
scans for those strings in the VM and the runtime, and a `__hoisted` check in the BEGIN prologue's
package-body split that could never fire (the parser never produces a hoisted shell).
