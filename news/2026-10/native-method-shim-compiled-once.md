# Native supply callbacks run as compiled closures

ADR-0055 section 8.3 (#14572), first slice. The `native_method_shim` callables the
supply machinery hands to taps, `done`/`quit` handlers and close callbacks used to
carry only an AST body, so every call went through `call_sub_value`'s tree-walk
carrier. A `t/` census counted 387 carrier calls, 303 of them these shims. The shim
body now reads its instance from a captured lexical instead of embedding it as a
literal, so it is identical for every shim of one `(method, has_param)` and is
compiled once per thread (`synthesized_closure_code`); the shim carries that chunk
and runs through the compiled closure path.
