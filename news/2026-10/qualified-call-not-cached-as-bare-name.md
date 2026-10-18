# A qualified call no longer changes what a bare call means

A module that called another module's same-named routine through its
qualified name (`RE::name()`) had every later bare `name()` in its own
package run `RE::name` instead of its own `sub name` (#13873). The `CallFunc`
slow path compiled the callee on the fly and cached it in `otf_call_cache`
under the routine's bare name for the calling package. It did this whatever
name the call site used.

The slow-path arms now pass the call site's name. A routine reached through a
package-qualified name is called the way `compile_and_call_qualified_def`
already calls one, so it is never cached as the meaning of the bare name.
With this fix (and the group-declaration one), BDD::Behave's
`t/shared-examples/shared-examples.rakutest` passes 13/13: `DSL`'s own
`registry()` no longer turns into `SharedExamples::registry()` after a
`shared-examples` call.
