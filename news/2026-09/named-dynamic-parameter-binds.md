# Named dynamic parameters (`:$*name`) accept `:name` and bind `$*name`

A named parameter with the `*` twigil (`-> :$*bli { ... }`, `sub g(:$*x)`) now matches the
caller's `:bli` / `:x` argument and binds the dynamic variable for nested calls. Previously
the twigil was kept as part of the accepted key, so a pointy block died with
`Unexpected named argument 'bli' passed` and a sub silently left `$*x` unbound. The twigil
strip now lives beside the existing `!`/`.` handling in every named-key derivation.
Closes #10284.
