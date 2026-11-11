# nqp::bindlexdyn finds its callers from a routine that takes arguments

`nqp::bindlexdyn` binds a dynamic its caller declared, so it needs the calling
frames. A routine that takes positional arguments usually runs on the light
call path, which skips pushing them, so `sub g($v) { nqp::bindlexdyn('$*x', $v) }`
died `Dynamic variable '$*x' not found` even when its caller had declared
`my $*x`. The op is now one of the caller readers, like `nqp::getlexcaller`,
that keep their routine off that path.

A routine's implicit `$_`, `$/` and `$!` always exist in the calling frame, so
binding them no longer dies when the caller never assigned one. That was the
failure of Qwiratry's `call-with-topic` (save the caller's topic, bind it,
call the body, restore it) in its aggregate tests (#17452). The caller does
not see such a topic bind yet (#17453).
