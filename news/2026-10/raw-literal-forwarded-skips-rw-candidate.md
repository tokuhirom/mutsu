# A literal forwarded through an `is raw` parameter no longer matches an `is rw` candidate

`method coerce($a is raw, $b is raw) { self.coerce-to($a, $b) }` called with a literal binds `$a` to
the value itself, with no Scalar behind it. Forwarding `$a` to a multi with an `is rw` candidate still
counted as passing a variable, so the `is rw` candidate was selected and then died with "Cannot assign
to an immutable value". Dispatch now treats such a forwarded parameter as a non-container, as rakudo
does (PDF::COS::Coercer). This is one step towards PDF's own `t/cos-*.t`, which still differ.
