# `nqp::unbox_*` refuses a type object

A type object boxes no value. `nqp::unbox_s(Str)` used to answer `(Str)`,
and `nqp::unbox_i(Int)` answered 0. Each now dies with MoarVM's message,
`Cannot unbox a type object (Int) to an int.` (#12911). `nqp::unbox_u` has its
own wording, `Cannot unbox a P6opaque (Int) type object`. The BOOT type
objects that an async op pushes as a null slot are refused the same way.
