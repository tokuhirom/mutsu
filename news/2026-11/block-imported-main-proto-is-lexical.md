# A `proto MAIN` imported in a block no longer replaces the program's MAIN

`MAIN` is the one routine an import never binds in a block's lexical import
set: the program reads `&MAIN` by name when it ends. So a module's exported
`MAIN` family lands under `GLOBAL::MAIN`. When the importing block closes, the
candidates the module declared were already dropped, but its `proto MAIN`
stayed. The program's own `sub MAIN` then ran behind that proto's body when
the program ended, and the proto's `{*}` found none of its candidates:
`Cannot resolve caller MAIN()`.

That is how rakudoc's `t/01-cmd.t` failed after all of its assertions had
passed. It tests `Rakudoc::CMD::MAIN` from a sub that does
`use Rakudoc::CMD`, next to the script's own `BEGIN sub MAIN(|) { }`. The
block's scope exit now drops a foreign `proto MAIN` together with its
candidates (`runtime::foreign_main`), so the file exits 0 as on rakudo.
