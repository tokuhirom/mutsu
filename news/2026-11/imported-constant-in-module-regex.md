# A module's imported constant is interpolated in its own regexes

A `constant` that a module pulls in with `use` lives in that module's unit pad, not in the
calling frame's env. A regex written inside the module (`s:g[ $SENTINEL ] = '"'`, `m/ $WORD /`)
read it as an undefined value, i.e. an empty pattern, so the substitution rewrote the wrong
characters. The regex interpolation fallbacks (`regex_package_chain_var_fallback`, and the
match-time `VarInterp` read) now also consult the module-scope table and the unit pad, and no
longer give up early for a sub running under GLOBAL. Found while running the `CSS::Minifier`
distribution, whose `:preserve-licenses` suite turned `!` into `"` inside quoted strings;
`t/03-golden.rakutest` now prints what `raku` prints.
