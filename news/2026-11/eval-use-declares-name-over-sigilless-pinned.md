# An EVAL'd `use X; X` reads the imported package, pinned by a test

#15806 reported that `"use Dep; Dep".EVAL` read an enclosing sigilless `\Dep` instead of the package the
EVAL's own `use` imported (Humming-Bird `t/opt/plugin-dbiish.rakutest`). On current `main` every shape in
the report agrees with rakudo: the plain case, the self-referential `my \Dep = try "use Dep; Dep".EVAL`,
and an EVAL inside a routine called from the scope that holds the sigilless. No code change was needed
(the EVAL-scope work of the last days fixed it as a side effect); `t/lang/eval-use-declares-name-over-sigilless.t`
now pins it, so a later change to bareword resolution cannot bring it back unnoticed.
