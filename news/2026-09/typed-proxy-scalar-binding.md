# Typed scalar bindings check Proxy values

A typed scalar bound with `:=` to a Proxy now checks the value returned by
`FETCH` while retaining the Proxy as its writable container. The check also
works when the source is another Proxy-bound variable, calls `FETCH` once, and
reports `X::TypeCheck::Binding` when the fetched value does not satisfy the
declared type, including when `FETCH` returns `Nil`.
