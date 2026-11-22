# `$CALLER::_ = v` and `$CALLER::_++` write the caller's topic

Under 6.c, `$_` is dynamic, so a routine can change its caller's topic:
`my sub modify { $CALLER::_++ }; $_ = 42; modify(); say $_` prints `43`.
mutsu read `$CALLER::_` correctly but lost the write. `set_caller_var` put the
new value into the caller-env snapshot and into the callee's own env, and the
return-time writeback skips `$_` on purpose, because each routine has its own
topic. So the caller never saw the change, and the callee's `$_` was
clobbered instead.

A write to `$CALLER::_` now goes the way `CALLER::LEXICAL::<$_> = v` already
went (`write_caller_topic`): into the call frame's saved caller env, which is
drained into the caller's slot on return. roast
`6.c/APPENDICES/A03-older-specs/01-misc.t` now passes its `$CALLER::_` group.
