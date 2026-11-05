# ADR-15760 Amendment 1 accepted

The maintainer accepted Amendment 1 of ADR-15760 (NameScope): slice 1 is split into 1a-1c, and the
uppercase-bareword row of #15791 moves to slice 2 as 1d. `Str.AST` will raise the same compile errors as
running the code, and the post-declared-type check lives in `src/rakuast`.
