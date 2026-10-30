# A module's END phaser sees the routines the module imported

An `END` block inside a module runs at program exit, when the running compunit is the
main script. Its calls to routines the module itself had `use`d (File::Temp's
`rmtree` from File::Directory::Tree) therefore died with `Unknown function`. The END
runner now runs the body as the declaring compunit's top-level code, so the module's
own import sets apply. Found while working Path::Finder, whose 14 test files now all
match `raku`.
