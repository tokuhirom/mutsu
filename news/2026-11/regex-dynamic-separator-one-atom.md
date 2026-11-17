# A spliced `%` separator is one atom

`[\w]+ % $*S` with `$*S = ";"` matched only `a;` of `"a;b;c"` (#17891). The
interpolation pre-pass splices a dynamic variable's value into the pattern
between two non-declarative interpolation marks. The `%` separator extractor
(`split_first_atom`) then took only the first mark as the separator atom, so
the separator parsed to nothing and the value was matched as an ordinary atom
after the quantifier. The whole marked span is now the one atom it replaced,
whatever the value's length.
