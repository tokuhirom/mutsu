# A role method whose body cannot see the role context takes the method fast path

Every `CArray[T]` element access ran the full method binder: `call_compiled_method_fast` (no env
copy, a light call frame, locals populated directly) was refused by three gates --
the `::?CLASS:D:` invocant against a role-mixed base, the `?CLASS:D` type capture of that invocant,
and the `role_param_bindings` that `dispatch_mixin_method_call` always attached (the role's type
parameters, `?CLASS`, the role package's statics).

`CompiledCode::ignores_role_context` now says when a body is a straight line of local reads and
`nqp::` ops that name no lexical and read neither `?CLASS` nor `?ROLE`
(`nqp::bindpos_n(self, $pos, $value)`). For such a body whose parameters are typed with built-in
types only, the three gates open and the mixin dispatch does not build the bindings. A body that
reads a role parameter, `::?CLASS` or a static keeps the full path
(`t/nativecall/carray-mixin-element-store.t` pins both).

Callgrind slope on a profiling build, second run: fill 82.3k -> 61.3k Ir per store; fill +
`.Array` 185.1k -> 148.8k Ir per element. Refs #16215.
