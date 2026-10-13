# The last coercion arms are method-table rows

`methods_0arg/dispatch_core_coerce.rs` no longer dispatches by name (ADR-11276 §9.94). Its last three blocks became rows or
went away. `Complex.Complex` of the type object is the type object, from the `Complex.Complex` row. `.serial` is `Any`'s row,
and every receiver reaches it, type objects included. The buffer reinterpretation block is gone. Like Rakudo, `Blob` declares
`.Buf` but not `.Blob`, so `Blob.new(1).Blob` and `"ab".encode.Blob` now throw `X::Method::NotFound`, while `.Buf` of a buffer and
`.Blob` of a `Buf` work as before. `Mu.serial` is `X::Method::NotFound`, as in Rakudo.
