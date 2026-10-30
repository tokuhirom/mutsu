# A `my method` called by name takes a variable argument as its invocant

`my method m { ... }` in a class body can be called as `m($obj, ...)`, with the
first argument as its invocant. When that argument was a scalar variable
(`m($o, 1)`, `m(self, 1)`, `m(\o, 1)`) the call site hands the callee a `VarRef`
wrapper, which was installed as the frame's `self` unchanged: `self` and every
`$!attr` read then saw an `Any` type object ("Cannot look up attributes in a
Any type object"). The implicit invocant is now unwrapped to the object it
holds. This is how the `annotations` distribution's `Binder.BIND` reaches its
`STORE` (`STORE SELF, $topic`), so its `Buffer` slots now fill.
