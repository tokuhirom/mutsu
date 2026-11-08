# A `$*x` read before a block-nested `my $*x` is a Failure, not Nil

`sub a { say $*M.^name; if True { my $*M = "inner" } }` printed `Nil`; raku prints
`Failure`. The not-found check excluded every `$*x` the chunk declares anywhere
(meant for the same-scope `X::Dynamic::Postdeclaration` case). It now only
excludes names declared in the chunk's own top scope
(`CompiledCode::dynamic_toplevel_declared_sym`), so a read before a nested
declaration is the ordinary dynamic-not-found Failure (#17113).
