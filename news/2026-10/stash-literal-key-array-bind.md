# `Pkg::<@a> := v` rebinds the package variable

`package Q { our @a }; Q::<@a> := [5, 6]; say @Q::a` printed `[]`: the stash
spelling of `@Q::a := [5, 6]` only rewrote `$`-keyed binds into the
qualified-variable form, so an `@`/`%` key fell through to an index-assign into
the throwaway hash `GetPseudoStash` builds, and the bind was lost.
`try_compile_named_package_stash_assign` now compiles an `@`/`%` literal-key
bind as `@Pkg::a := v` / `%Pkg::h := v`. In expression context
(`my $r = (Q::<@a> := [7])`) the parser did not even mark the stash subscript
as a bind and treated it as an assignment; a pseudo-stash target now takes the
same bind-marked path as `%h<k> := v` (#10546).
