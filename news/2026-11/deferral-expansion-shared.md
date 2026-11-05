# The deferral expansion of a method is built once per class

Every method call that needs a dispatch frame (a `multi method`, or a method
the user overrides) rebuilt the structural deferral expansion of
`(class, method)`: it cloned every candidate `MethodDef` of the MRO, ranked
them and dropped flattened role duplicates, on each call (~1,400 `MethodDef`
clones per AttrX::Mooish iteration). The expansion is a function of the
registry alone, so `deferral_expansion_shared` keeps one `Arc` per
`(class, method)` while the registry's `type_stamp` is unchanged, and
`matched_deferral_candidates` clones only the candidates it keeps.

Part of #15688.
