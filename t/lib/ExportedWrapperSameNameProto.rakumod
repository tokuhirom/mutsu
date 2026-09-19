# #8798: an exported wrapper whose OWN name matches an imported proto/multi
# family it uses internally. The `use` of the dependency lives inside the
# wrapper's body (not at the module's top level), so it re-imports the raw
# family on every call — the shape `P5getgrnam`'s `getgrgid`/`getgrnam`/
# `getgrent` wrappers have in the real ecosystem distribution.
my sub wrapped-thing(Int() $n) is export {
    use ExportedWrapperSameNameProtoInner;
    "wrapped:" ~ wrapped-thing($n);
}
