# A role's `is export(:TAG)` declares its named tags

`role R is export(:T) { }` in a module left the tag undeclared, so
`use M :T` died with "no such tag 'T' declared" (#9372). The role parser (both
the block form and `unit role`) recorded only `DEFAULT` for every `is export`
trait and threw its argument list away, while the class parser already read
each `:TAG`. The runtime side was correct all along: `RegisterRole` hands the
declaration's `export_tags` to `register_exported_var`, exactly as for a class
or a subset.

The tag parsing now lives in one helper, `class_decl::push_export_tags`, shared
by the class, role and `unit role` parsers: a bare `is export` records
`DEFAULT`, a tagged one records each named tag (and is importable under
`:ALL`).

This unblocks PDF 0.6.15, whose `PDF::COS::Tie` exports
`my role COSDictAttrHOW does COSAttrHOW is export(:COSDictAttrHOW)` and whose
`PDF::COS::Tie::Hash` imports it by that tag, so every module importing
`PDF::COS::Tie::Hash` (most of PDF::Font::Loader) failed to load.

Pin: `t/modules/import-export/role-is-export-tag.t`.
