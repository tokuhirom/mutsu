# Thread-clone backtraces retain their spawn origin

An exception raised by an anonymous callback on a worker interpreter could have
an empty rendered backtrace. The callback's sole frame is intentionally omitted
from concise backtraces, while thread clones previously had neither a mainline
frame nor another source location to show.

Each thread clone now records the source file and line at which it was spawned.
When an anonymous worker callback is the bottom frame, the backtrace appends one
synthetic enclosing frame from that recorded origin. Located worker entry blocks
continue to render only their own frame, preserving the existing guarantee that
`Promise.start` failures do not receive a duplicate frame.

`t/thread-clone-backtrace-origin.t` covers both the pending `.then` callback
that previously produced an empty backtrace and the non-duplication regression.
