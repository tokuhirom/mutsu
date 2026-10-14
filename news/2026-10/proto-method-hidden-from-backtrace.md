# `proto method ... is hidden-from-backtrace` hides the proto's frame

The trait was applied to `multi method` candidates and plain methods but dropped on a `proto method`,
so `Backtrace.new.list.grep(!*.is-hidden)` still listed the proto's `log` frame. Lumberjack's `Logger`
role builds its messages from that backtrace and reported the wrong method name (`log` instead of the
caller), failing `t/060-dispatcher-console.t` and `t/070-dispatcher-file.t`.
