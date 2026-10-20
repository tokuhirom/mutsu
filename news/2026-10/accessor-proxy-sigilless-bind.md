# Binding `$.x` to a Proxy-returning accessor leaves the attribute alone

`my \e = $.encoded`, where `method encoded is rw` returns a `Proxy`, used to
treat `$.encoded` as an alias of the attribute `$!encoded`. The bind tags its
source with the variable it names, so the first read of `e` stored the Proxy
into `$!encoded`. The Proxy's `FETCH` reads `$!encoded`, so it then recursed
until the stack ran out. `$.x` names whatever container `self.x` returns, so
the binding is now that Proxy itself. An `is rw` attribute accessor, which
returns the attribute's own container, still aliases the attribute as before.

PDF's `PDF::COS::Stream.content` binds `my \encoded = $.encoded`. Because of
this bug, writing any stream object died with "Too deep recursion" (#12907).
