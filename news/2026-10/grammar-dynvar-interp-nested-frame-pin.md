# Pin: interpolated `$*dyn` reads the enclosing rule frame after a nested rule returns

A rule whose pattern interpolates a `$*` dynamic variable (`<!before $*c>`) read
the nested rule's `:my $*c` after that rule had returned (#13641). That no longer
happens: the reduction in the issue matches Rakudo, and Rainbow's
`t/sub-langs.rakutest` passes 4/4. `t/grammar/grammar-dynvar-interp-nested-frame.t`
pins it.
