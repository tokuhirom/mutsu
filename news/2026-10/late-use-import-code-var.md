# `&name` above a later top-level `use` reads the imported routine

The fallback import chain that lets a bareword call above a unit-level `use`
reach the routine it imports now covers the code-variable read `&name` too, so
`say &greet4.name; use LateOps3;` finds `greet4`, as rakudo does.
