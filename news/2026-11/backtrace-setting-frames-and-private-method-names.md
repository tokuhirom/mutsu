# Backtrace: setting `throw`/`die` frames and private method names

Two shape differences from rakudo in `Exception.backtrace.list` (#19244, found
through Rakuish's `errors full`, which skips `SETTING::` frames and looks the
other frames' lines up in their source files):

- The leading `throw` and `die` frames now sit at rakudo 2026.11's setting
  positions, `SETTING::src/core.c/Exception.rakumod:65` and
  `SETTING::src/core.c/control.rakumod:253`, instead of the user's own file
  and line.
- A private method's frame is `p`, not `!p`, in `.subname` and in the
  rendered `in method p at ...` line.

Still open in #19244: `do { }` pushes no block frame, and a module frame's
`.file` lacks rakudo's ` (Module)` suffix (a maintainer decision).
