An imported routine can now be shadowed by a same-named local `sub` declaration,
including routines installed by a module's `sub EXPORT`. A second local
declaration in the same scope still raises `X::Redeclaration`. (#8214)
