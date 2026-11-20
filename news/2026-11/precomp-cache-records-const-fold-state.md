# The precomp cache records whether constant folding was on

`Compiler::const_fold_enabled` turns constant folding off once any user infix
operator is registered anywhere in the process. That process-global answer was
not a recorded compile input, so a module compiled by a cold process (its
`constant` reads folded) was served from the precomp cache to a process that had
already registered an importer's `infix:<...>`, where a fresh compile does not
fold. `MUTSU_PRECOMP_VERIFY` reported it as a difference in `free_var_decl_slots`
(`t/modules/compunit/module-scan-precomp-cache.t`), and a program redefining an
operator could run folded code. The state is now an `InputKey`, so a differing
process invalidates the entry. Closes #18270.
