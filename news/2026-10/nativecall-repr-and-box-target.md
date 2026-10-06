# The `NativeCall` REPR and `is box_target`

A class declared `is repr<NativeCall>` (upstream NativeCall's `my class
Callsite`) now reports `NativeCall` for its type object and its instances
(#11209, ADR-11203 §2.4), and `is box_target` is a core attribute trait
instead of an "unknown trait" error.

`is box_target` names the attribute whose body an object *is* as far as
MoarVM's native ops go. Upstream keeps a routine's built call in
`has Callsite $!call is box_target`, a role attribute mixed into the routine,
and `Native!setup` does `return if nqp::unbox_i($!call)`. mutsu records the
attribute per declaring class or role, allocates a class-typed box target when
the object is created (the parser seeds `Type.CREATE`, as MoarVM inlines the
body into the object, so `$!call` reads back as a concrete `Callsite` before
anything is built), and lets `nqp::buildnativecall` and `nqp::nativecall`
operate on an object's box target. Until now the routine itself was the
callsite and `nqp::unbox_i($!call)` stayed 0, so every call of an upstream
`is native` routine rebuilt its callsite.

`t/nativecall/nativecall-box-target-callsite.t` pins the shape against
Rakudo: a role mixed into a routine, a plain class, two objects that must not
share a callsite, and a callsite with no holder.

`nqp::box_*` into a class with a box target and `nqp::unbox_*` through one
are not delegated yet; they need the generic "box into a class" rule that the
`CStr` REPR also needs, which stays open on #11209 together with `CStruct.new`
storage, `CUnion` and `CPPStruct`.
