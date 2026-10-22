# An `is rw` parameter relayed onward keeps aliasing its container

An `is rw` parameter of a light-called sub lives in a shared cell in its local
slot only. Passing it on to a typed `is rw`/`is raw` sub, to a method with an
`is raw` parameter, or to a method whose rw parameter is `$_`, bound a fresh
cell and the callee's write never reached the original variable or hash
element. The call sites now publish the caller's cell into env before the
general binder runs, and a rw `$_` parameter takes the shared-cell binding like
any other. PDF 0.6.17 `cos-tie-entry` (enum assigned to a `PDF::COS::Name`
entry) now passes.
