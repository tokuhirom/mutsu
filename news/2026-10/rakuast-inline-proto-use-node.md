# An inline `proto` argument of `use` is a typed parser node

`use Foo, proto sub MAIN(|) {*}` is `ParsedStmt::InlineProtoUse`, no longer a `SyntheticBlock` that the
statement converter matched by the shape of its two statements. A coverage run over the whole corpus
also removed the `package_origins` branch that looked for a `SyntheticBlock` around a class
declaration; `class C is export { }` reaches it as a `PackageHeader` node (ADR-10723 §6.118).
`convert_stmt_node` no longer mentions `SyntheticBlock`.
