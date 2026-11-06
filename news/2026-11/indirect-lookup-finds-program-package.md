# `::($name)` from a module finds a package the program declared

`::('MyPackageClass')` evaluated inside a module's routine returned a Failure when
`MyPackageClass` is a `module`/`package` declared by the main program (classes were
found, packages were not): a bare package name was only recognised through the frame
env, which another compunit's routine does not share. A top-level, non-`my`
`package`/`module` is now found through its persistent kind record. UML::Translators
(`to-plant-uml-spec('MyPackageClass')`) now matches `raku` on both test files.
