# Method calls no longer clone a class's attribute list

`seed_attr_value`, the rw-accessor probe in `call_method_mut_with_values` and
`.Capture` of an instance each rebuilt the class's merged attribute list
(`collect_class_attributes`), cloning every `ClassAttributeDef` and its
`String`s, on every call: about 90 clones and 300 string allocations per
AttrX::Mooish iteration. Read-only callers now share one `Arc` list per class,
kept while the registry's `type_stamp` is unchanged (any class, role or enum
write drops it), via `Interpreter::class_attributes_shared`.

Part of #15688.
