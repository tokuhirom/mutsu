# Assigning to an `nqp::` op call writes through the returned container

`nqp::atposref_i(@a, 0) = 5` used to die with `Unknown call: nqp::atposref_i`.
The parser lowers `CALL(...) = value` to an lvalue routine call that looks the
callee up by name, and an `nqp::` op is not a routine. Such a call is now
treated like mutsu's internal `__mutsu_*` builtins. The parser evaluates the op
and assigns through the container it returns, so the `atposref_i` /
`atposref_n` / `atposref_s` lvalues that upstream NativeCall's `CArray` roles
use now store into the array. An op that returns a plain value dies
`X::Assignment::RO`, as rakudo does (#11451).

A related gap remains: `nqp::atpos` on a high-level `Array` should die with
"does not support positional operations". That is #11711.
