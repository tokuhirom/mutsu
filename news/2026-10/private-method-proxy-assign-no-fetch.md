# Private-method assignment through a Proxy no longer FETCHes

`self!m() = v` over a private `is rw` method returning a `Proxy` ran STORE and
then FETCH, because the call site fetched the container the assignment handed
back. The assignment now answers the stored value for a `Proxy` result, as the
public-method form does, so a sink-context assignment fires STORE alone (#12322).
