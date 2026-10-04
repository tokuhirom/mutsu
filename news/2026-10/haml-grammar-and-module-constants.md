# A grammar named `Grammar`, and a unit module's constants beside a package-less module's

Two gaps found by the Template::HAML suite. `grammar Grammar { }` died with
"cannot inherit from 'CORE::Grammar' because it is unknown": the implicit core parent of a class that
shadows its own core type is spelled `CORE::`, and the parent validation did not accept that
spelling for cataloged core types. And a `unit module` whose routines read a file-scope
`constant` died with "Undeclared name" once another, package-less module that had been loaded
through a nested `use` had published a constant of the same name; the bare-name visibility gate hid
the module's own declaration, and when it did resolve, the other module's value won. A module's own
constant now wins inside its routines.
