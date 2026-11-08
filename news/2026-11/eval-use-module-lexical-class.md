# A module's `my class` stays visible to its own closures when loaded by a `use` inside an EVAL

`EVAL 'use M; class T { }; install-on(T); T.method-added-by-M'` died with `Undeclared name:
Hidden` when the module `M` declared `my class Hidden` and installed a method using it via
`^add_method`. The `use` under an `EVAL` binds the module's lexical types in the EVAL's import
region, a tier the closure's static link to the module's compunit skips. The load now records
each lexical class / role a module's compunit declared, and a bare word that the module's own
code cannot otherwise resolve is read from that record (#17129). `t/modules/add-method-module-lexical-type.t`
joins the `EVAL($ast)` ratchet.
