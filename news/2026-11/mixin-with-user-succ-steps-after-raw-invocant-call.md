# `++` on a boxed mixin keeps its type after a raw-invocant call

With role methods now arming ADR-0067's raw-invocant arrival (#16100), a call
like `$ptr.deref` (`deref(::?CLASS:D \ptr:)`) parks the variable in a shared
cell. `$ptr++` then went through the cell's generic atomic step, which stepped
the boxed `Pointer` as a bare `Int` and dropped the mixin, so NativeHelpers::Blob's
`t/03-pointer.t` lost `deref` after `++`. The atomic step now declines a `Mixin`
(as it already did an `Instance`), and the user `succ`/`pred` route runs.
Pinned by `t/oo/mixin-with-user-succ-steps-after-raw-invocant-call.t`.
