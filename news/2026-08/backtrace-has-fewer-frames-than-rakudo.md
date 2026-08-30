# Backtraces include native die frames and explicit try blocks

The documented nested-block example now reports seven frames, matching Rakudo
instead of reporting four.

The gap had two independent causes. The source block of an explicit `try` was
compiled as a trapping `TryCatch` region but was not marked as an anonymous
callframe. In addition, native `die` dispatch bypasses the Raku setting routines
that logically contribute `throw` and `die` frames. Explicit `try` regions now
carry their block-frame marker into the VM, and the structured backtrace builder
can prepend multiple native setting-routine frames. The `Die` opcode supplies
`throw` and `die`, marks them for `.is-setting` and the standard backtrace
filters, and extends the existing mechanism used by explicit `.throw`.

The synthetic native frames only affect the structured frame list. Concise
backtrace text continues to show mutsu's useful user-code locations without
inventing setting source paths.

`t/backtrace-block-frames.t` pins the seven-frame count, the leading native
routine names, the anonymous `try` frame, and the final `<unit>` frame.
