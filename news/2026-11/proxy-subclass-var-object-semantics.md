# A Proxy subclass's `.VAR` object is an instance of its class

Three differences from rakudo around `class P is Proxy` are gone:

- `$p.VAR ~~ P`, `$p.VAR ~~ Proxy` and `$p.VAR.isa(P)` are `True`. A smartmatch
  read its left side through FETCH even when it was the `.VAR` object, which is
  the Proxy itself rather than a container; and a type check of a Proxy
  subclass now walks the subclass's MRO (and roles) in both the smartmatch
  and the `isa` row.
- Inside a Proxy subclass's method `self` is the Proxy as a container: reading
  it FETCHes (`self.^name` is the fetched value's type, as rakudo
  decontainerizes the invocant on use) while `self.VAR` is the Proxy.
- `Proxy.new(FETCH => ..., STORE => ...)` builds the container without
  `BUILDALL`: an attribute's default expression is not evaluated (an untyped
  attribute reads `Any`, a typed one its type object, an `is default` one its
  default, inherited ones included), and any named argument other than `FETCH`
  and `STORE` dies with `Unexpected named argument '...' passed`.
