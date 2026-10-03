# The six `nqp::` FFI ops upstream NativeCall is built on

`nqp::buildnativecall`, `nqp::nativecall`, `nqp::nativecallcast`, `nqp::nativecallsizeof`,
`nqp::nativecallglobal` and `nqp::nativecallrefresh` now exist (#11211, ADR-11203). They are
MoarVM's FFI layer, which upstream `NativeCall.rakumod` calls once it has worked out each
parameter's type code. In mutsu they run on the libloading + libffi machinery the native `is
native` path already uses. `buildnativecall` reads upstream's argument- and return-info hashes
(`type`, `rw`, `free_str`, `typeobj`, `callback_args`, `entry_point`) into a call descriptor, and
`nativecall` makes the call and boxes the result as its `$rettype`. A `void` function and a NULL
`char*` answer with the return type object, as on MoarVM. The native provider's
`nativesizeof`/`nativecast` share their bodies with the two matching ops. `nativesizeof` also
learned the size of a `native` type declared with `is nativesize(N)`.

The vendored upstream module (loaded under a renamed namespace by
`scripts/nativecall-upstream-trial.sh`) now gets past `load` and `nativesizeof`. Those two steps
used to stop on the missing `nqp::nativecallsizeof`.

Getting there also fixed `.?method` on a routine. It used to return the Sub that mutsu's
last-resort method composition builds for a callable, where rakudo returns Nil. Upstream's
`self.?native_call_convention || ''` failed its `str` type check because of it.
