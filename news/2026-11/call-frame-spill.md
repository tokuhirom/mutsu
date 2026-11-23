# A call frame saves the usually-empty caller registers only when they are not empty

Every call frame (`VmCallFrame`) saved six caller registers of the
interpreter by moving them out of it, and restored them on return:
- the upvalue array;
- the local bind pairs;
- the closure capture's authoritative and owned name sets;
- the pending caller-variable writebacks;
- the `is rw` parameter rebinds.

In almost every activation all six are empty, yet each call still moved six
empty containers into the call stack and back out, and dropped them.

They now live in one boxed `VmCallFrameSpill`, which is allocated only when one
of them is non-empty. A frame without one clears, on return, only the
registers its callee filled (#18267 stage 3). The record itself moved out of
`src/vm.rs` into `src/vm/vm_call_frame.rs`.

The call-path harness's `plain-call` input (two method calls per iteration)
goes from 26,685 to 26,195 instructions per iteration, about 245 fewer per
method call.
