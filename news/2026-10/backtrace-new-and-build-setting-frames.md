# Backtrace lists Rakudo's setting frames for `Backtrace.new` and BUILD

`Backtrace.new.list` now starts with Rakudo's own `Backtrace.new` setting frame, and a
`BUILD`/`TWEAK` submethod frame is followed by the `POPULATE` and `Mu.new` setting frames
Rakudo shows. Code that finds its caller by skipping to the first non-Backtrace setting frame
(Proc::Easier's `caller-line` / `caller-file`) now gets the call site instead of its own module.
