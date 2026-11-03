# A type a used module declares is not post-declared in the using unit

The "Illegally post-declared type" check for every unit knew only the unit's own
declarations, so a module that did `use Stub;` (whose first line is the stub
`role R {...}`), used `R`, and declared `role R { ... }` further down died with
`Illegally post-declared type: R` (BTree 0.0.2, #16259). A type name a `use`d
module declared, stubs included, is the unit's from the `use` on; the parser
records those names apart from the unit's own (`is_module_imported_type`), and
the check skips them. A unit's own post-declaration is still the compile error.
