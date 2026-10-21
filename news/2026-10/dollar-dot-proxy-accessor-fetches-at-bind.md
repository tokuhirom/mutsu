# `my \e = $.x` over a Proxy accessor fetches at the bind

`$.x` is `self.x` itemized in Rakudo, so binding a sigilless name to it reads the
container: when `method x is rw` returns a `Proxy`, its FETCH runs at the `my \e = $.x`
statement. mutsu kept the Proxy unevaluated, so a FETCH side effect that the next statement
depends on (PDF::COS::Stream's `content` sets `/Length` while fetching `$.encoded`) had not
happened yet. PDF 0.6.17's `t/indobj-stream.t` now passes all 27 tests (#12907).
