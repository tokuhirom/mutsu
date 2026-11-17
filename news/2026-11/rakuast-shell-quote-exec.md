# RakuAST: a shell quote is a QuotedString with the `exec` processor

`qx/.../`, `qqx/.../`, `q:x/.../` and `qq:x/.../` now view as rakudo's RakuAST
frontend views them (#17950): a `RakuAST::QuotedString` whose processors are
`("exec",)`, holding the command's segments. A `qqx` keeps its interpolated
variables as segments. They used to view as a call to the internal `QX`
routine around a plain quote. A `QX(...)` the program writes itself is still a
`Call::Name`.

The parser now marks a shell quote's command run with its own routine name,
`qx`. A program cannot write that as a call, because `qx(...)` parses as a
quote. The view maps the mark to the `exec` processor, the same way the
`format` processor is handled, and the lowering maps `exec` back. Such a quote
round-trips through `EVAL`, and a hand-built `exec` quote runs its command.
