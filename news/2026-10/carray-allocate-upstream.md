# `CArray[T].allocate` runs upstream NativeCall's method

`CArray[int32].allocate(4)` and its kin were answered by a by-name gate that pre-sized the array
natively. The vendored upstream `NativeCall::Types` declares `allocate` on its typed `CArray`
roles, and mutsu runs it: the gate is gone (ADR-11276 §9.193), with the same arrays as before.
