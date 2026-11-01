# A class body's bound `@`/`%` lexical is the class's own static

`unit class X; my %h := Hash.new; method m { %h }` in two modules made the second
module's methods read the first module's hash. A class body's declared statics
are collected from its top-level `VarDecl`s, but the parser wraps a bound
`@`/`%` declaration (`my %h := ...`) in a `SyntheticBlock`, which that scan did
not open, so the name was never persisted as the class's static and its methods
fell back to a same-named lexical of whichever module loaded first. The scan now
reads the body through `scope_statements`. Net::BGP's `Notify::Header` and
`Notify::Open` both bind `%error-subcodes`, so a bad BGP marker was answered with
the Open `Unsupported-Version` NOTIFICATION instead of `Connection-Not-Syncronized`.
