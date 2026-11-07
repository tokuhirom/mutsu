# `OUTERS::` inside a method body sees the declaring lexical chain

A class/role method body was compiled with no enclosing scope chain, so
`$OUTERS::x` read the method's own shadowing parameter and `$OUTERS::x = v`
had no cell to write into (Jupyter::Kernel's sandbox hands its supplier to a
file-scope `state` that way). The main-pass method compiler now receives the
declaring chain for `OUTER::`/`OUTERS::` resolution. Also `@OUTERS::a` /
`%OUTERS::h` reads (a missing twin of `@OUTER::a`) now resolve instead of
reading an empty container. (#17003)
