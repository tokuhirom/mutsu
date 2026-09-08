# Stash `BIND-KEY` now installs containers

`Stash.BIND-KEY` now binds package symbols and caller lexicals to the supplied
container. Repeated `CALLER::` components address the corresponding caller
frame, so modules such as P5tie can install a `Proxy` whose `FETCH` and `STORE`
callbacks remain active after the binding routine returns.

Package stash bindings also preserve the source container when the value is a
variable, matching the aliasing behavior of hash-element `BIND-KEY`.
