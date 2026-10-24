# RakuAST Stage 2: package headers are a parser node

A `class` / `grammar` / `module` / `package` / `role` with `:ver<..>` / `:auth<..>` /
`:api<..>` adverbs or `is export` is kept as `ParsedStmt::PackageHeader` (the declaration as
written plus its header). `ast::package_header::wrap` is its lowering; `.AST` renders the node
directly, so the `convert.rs` arm that recognized the parser's `__MUTSU_SET_META__` /
`__MUTSU_EXPORT_TYPE__` wrapper is deleted. Doc-comment attachment, the module export scan and
the package origins read through the node. Tracked in #14940.
