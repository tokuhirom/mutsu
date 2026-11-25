# mutsu builds on macOS again: the descriptor-forward pipe uses the shared helper

A release smoke run on 2026-11-25 found both macOS release targets failing to compile.
`Proc::Async::Pipe.native-descriptor`'s forwarding pipe called `libc::pipe2`, which does not
exist on Apple platforms. No pull-request CI job builds for macOS, so the break would have shown
first at the next `v*` tag.

The pipe now comes from `runtime::cloexec_pipe`, the helper the other runtime-owned pipes already
use. It calls `pipe2(O_CLOEXEC)` where the platform has it and otherwise `pipe(2)` followed by
`FD_CLOEXEC`, so the descriptor stays close-on-exec everywhere.
