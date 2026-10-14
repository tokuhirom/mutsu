# Types after `unit module A::B is export;` are qualified into the module

`unit module A::B is export;` parses to a header wrapper around the package
declaration, and the compiler's unit-package detection (the runtime package switch
and the type-shell hoist) only looked at bare `Package` statements. A module file
declaring `grammar INI` after it therefore registered the grammar as a global
`INI`, and `INI.parse` inside the module failed with `No such method 'TOP'`.
Both places now look through the wrapper. This fixes `App::RaCoCo`'s
`t/18-config-file.rakutest` and `t/20-cli-main.rakutest`.
