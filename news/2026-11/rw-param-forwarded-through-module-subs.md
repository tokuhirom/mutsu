# An `is rw` parameter forwarded between module subs keeps the write

An imported routine that handed its own `is rw` parameter to another routine of
its module (`sub parse-value(str $t, int $pos is rw) { ... parse-integer($t,
$pos) }`) lost the callee's write whenever the call went through a by-name site
that did not publish the caller's shared cell first: the first call of a callee
that was not compiled yet, and every later call through the cached dispatch.
The remaining by-name call sites now publish the `is rw` argument cells before
binding, as the others already did. With #16377's fix, TOML 3's `t/01-valid.t`,
`t/03-encoder.t` and `t/04-nqp.t` match rakudo (66/66/102) (#16384).
