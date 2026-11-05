unit module DifferentlyNamed:ver<0.0.7>:auth<zef:test>:api<2>;

# Fixture for t/modules/import-export/export-class-unit-module-name-differs-from-path.t
# (from Qt::QtWidgets): the file is `UnitNameMismatchFile.rakumod` but the unit
# module is `DifferentlyNamed`, and it carries version/auth/api adverbs.
class MismatchObj is export { method hi { "obj" } }
role MismatchRole is export { method r { "role" } }
sub mismatch-helper() is export { "helped" }
