# Backtraces name method frames `in method`

An exception raised in a method printed `in sub m at ...`. rakudo prints
`in method m at ...`, and `in submethod s at ...` for a submethod. The routine
frames already recorded whether they were methods, and the warning renderer
used that, but the text an exception carries (from `die`, and from any other
run-time error) and `Backtrace::Frame.Str` always wrote `sub`. Both now name
the frame's kind, and `Backtrace::Frame` carries `is-submethod` beside
`is-method`. The synthetic setting frame for `Exception.throw` is a method too.

A parameter binding failure of a method call (`B.m("a")` against
`method m(Int $x)`) was reported as if it happened in the caller: the method's
frame is only pushed once its parameters bind. It now carries the backtrace it
would have inside the method, at the method's declaration line, as subs
already did.
