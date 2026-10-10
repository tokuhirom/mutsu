# A Proxy handed out by an accessor binds to `is raw` / `is rw` / sigilless parameters

A typed `is raw` or `is rw` parameter rejected the Proxy an `is rw` accessor
returned (`expected Str but got Proxy`), because the Proxy arrived wrapped in a
container and the FETCHed-value type check missed it. The binder now looks
through the container, both for the type check and for keeping the Proxy bound,
so a sigilless parameter assigning to it fires the Proxy's STORE (#12592).
