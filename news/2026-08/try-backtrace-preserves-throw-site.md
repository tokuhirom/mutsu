# `try` backtraces now report the actual throw site

`$!.backtrace` after a `try` block reported the line containing `try {`, even
when `die` executed several lines later. A bare top-level `die` was already
correct; only `try` bodies lost the location before their exception reached
`$!`.

The parser intentionally left source-line markers out of ordinary expression
blocks, so every opcode in a `try` body inherited the outer statement's line.
The VM correctly captured the backtrace at the `die` opcode, but that opcode
had been tagged with the `try` line. `try` now uses a tracked block parser that
records the line of each body statement, while ordinary expression blocks keep
their existing line-tracking behavior.

`t/try-backtrace-throw-site.t` covers both the structured frame and the
rendered backtrace, with a preceding statement in the body to ensure the
reported line is the `die` line rather than the block's opening line.
