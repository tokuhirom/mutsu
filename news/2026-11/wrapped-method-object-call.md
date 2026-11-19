# A wrapped Method object called as a value runs its wrappers

`$m.wrap(...)` on a Method object wrapped the ordinary call `$obj.m`, but
invoking the Method object itself (`$obj.$m()`, `$m($obj)`) ran the bare
method: that call goes to the owner-bound qualified call (`W::m`), whose
dispatch never looked at the method's wrap chain. The qualified dispatch now
enters the chain the way ordinary dispatch does, with the owner's candidate as
the chain's winner, so `callsame`/`callwith` in a wrapper reach the owner's
method even when the invocant's class overrides it (#18049).
