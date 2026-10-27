# `is hidden-from-backtrace` is a field of the routine declaration

The trait was an internal `__hidden_from_backtrace` entry of a declaration's `custom_traits`, which the
RakuAST converter had to exempt from its "internal trait" refusal and five readers scanned for by name.
`SubDecl`, `MethodDecl` and `ProtoDecl` now carry `is_hidden_from_backtrace`, and the marker is gone.
