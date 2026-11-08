# `visit-children` of a declaration visits the initializer before the `shape`

`my @a[3] = 1,2,3` / `my Int %h{Str} = 1`: rakudo's `visit-children` reaches the `Initializer`
before the `shape`, although `.raku` lists the shape first. mutsu followed the field order;
the child walk now swaps the two for a simple variable declaration (#15780).
