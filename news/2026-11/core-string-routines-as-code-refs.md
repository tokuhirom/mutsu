# `&fc`, `&tclc`, `&wordcase`, `&samecase`, `&ords`, `&samemark`, `&trim-leading`, `&trim-trailing`, `&index`, `&rindex`, `&indices` and `&split` are first-class routines

These core subs were callable by name but `&name` was undefined (`Nil`), so
`:as(&fc)` passed `Nil` and the receiving `:&as` parameter could not be called
(`Unknown function: as`). They are now exposed like `&pick` / `&reduce`.
Found by running the Iter::Able distribution (`t/18-is-all-same`).
