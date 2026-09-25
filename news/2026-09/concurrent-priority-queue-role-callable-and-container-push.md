# Custom positional containers keep role callable parameters and method dispatch

`Concurrent::PriorityQueue` uses a parameterized role with a callable `:&cmp`
parameter and binds the role to `@` variables. Mutsu previously lost the
callable parameter's `&` alias during role composition, so a role method that
forwarded `:&cmp` to `Array::Sorted::Util` could not resolve the callable.

The array `push` fast path also trusted the name-keyed environment mirror. A
variable trait such as `my @q is Queue` can replace the declaration's local
slot with a custom positional container while that mirror still contains the
initializer array; captured `@q.push` then bypassed the custom method. The
opcode now carries the declaration slot and dispatches through a custom
container before taking the native array path. Generic `@` mutators likewise
only take their native-array shortcut for actual `Array` values, preserving
custom positional method dispatch.

Pinned by `t/oo/role/role-callable-param-and-custom-container.t`. This was found
while making `Concurrent::PriorityQueue` 0.0.2's two-test suite pass under
mutsu. Its basic test now reaches parity; the concurrent test is still blocked
inside the standard sandbox by mutsu's fixed per-user-thread stack reservation,
tracked in #9377. The exact concurrent test passes with the release binary
outside that sandbox.
