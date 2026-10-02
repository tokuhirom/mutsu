# `my class ... is export` in an inline module is exported

`module M { my class CC is export { ... } }; import M` died with `No exports
found for module: M`, while rakudo imports `CC`. A lexical class can't carry
the runtime `__MUTSU_EXPORT_TYPE__` call, because the synthetic block would
end its scope. So the parser leaves only an `__mutsu_export_type` marker on
the declaration, and only a module *file*'s load scan (and a role body)
consumed it. An inline module never published the class (#10557).

Class registration (`exec_register_class_op`) now publishes a lexical class
that carries the marker, with its tags, the same way an exported `role` is
recorded. This covers a `my class` declared directly in the module body and
one declared inside a routine of the module.

Pinned by `t/modules/import-export/inline-module-lexical-class-export.t`.
