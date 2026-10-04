# The compiled-bytecode cache is on by default

ADR-11756 step 5. A module's mainline compile is now written to the
precompilation cache and served to the next process without setting anything.
`MUTSU_PRECOMP_BYTECODE=0` turns it off, and so does `--no-precomp`.
`MUTSU_PRECOMP_VERIFY=1` and `MUTSU_PRECOMP_TRACE=1` work as before.

Before this, nothing pruned the `.code` files: the cache prune only counted the
`.bin` AST entries. It now counts both kinds, so the cache directory stays
bounded.

The nightly stress workflow gained a `precomp-verify` job. It runs `t/` and the
roast whitelist with `MUTSU_PRECOMP_VERIFY=1` over one cache that starts empty,
so every hit after a module's first load is checked byte for byte against a
fresh compile.

Loading `Test` (`use Test; ok 1;` minus an empty script, release build, warm
cache) drops from about 85M to about 65M instructions for every test file.
Routine registration (about 23.5M) is the next target (#11756).
