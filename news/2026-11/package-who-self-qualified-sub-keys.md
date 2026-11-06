# `sub M::e2` / `our sub M::o2` inside `module M` in the package stash

A sub named with its enclosing package's own name is no longer listed as a bare
`&e2` member of that package because of its `EXPORT::<tag>::` registry key, and
`our sub M::o2` is now listed as `&M::o2`, as rakudo does (#16819).
