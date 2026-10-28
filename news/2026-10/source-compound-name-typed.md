# A compound declaration's source spelling and parent spellings are typed fields

When the compiler package-qualified a `class` / `role` name (`class M::R` inside a package) or a
class's parent names, it appended `__source_compound_name` / `__parent_spelling` pseudo-traits to the
declaration's `custom_traits`, with the spelling as a literal trait argument, and registration
decoded them back. They are `PackageMarks::source_compound_name` and `PackageMarks::parent_spellings`
now, copied onto the class and role plans.

Deleted: the two pseudo-trait pushes in `qualify_decl_name`, the argument decoders
(`source_compound_name`, `parent_spelling` scanning trait names and `Pair` literals) in the VM, and
the `custom_traits` scans for the marker name in `add_class_decl_plan` / `add_role_decl_plan`.
