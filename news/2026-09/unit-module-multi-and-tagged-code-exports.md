# Unit-module multi and tagged code exports are importable

Imports from a `unit module` now retain every candidate of an exported multi,
including when the file's declared module name differs from its provided path.
Tagged code-variable exports are also available under the requested module
name.
