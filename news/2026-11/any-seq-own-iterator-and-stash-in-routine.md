# `.Seq` reads a class's own `iterator`; `::<M>` inside a routine

Two gaps found with BinaryHeap (its `t/05-representation` now matches rakudo;
`t/01`-`t/04` need the issues linked from the PR).

- `Any.Seq` is `Seq.new(self.iterator)`, but a class declaring both `iterator`
  and a `list` that calls `.Seq` was answered through its own `list`, which
  recursed until "Too deep recursion". A class's own `iterator` now wins for
  `Seq` (and the other `Any` methods that read `self.iterator`).
- `::<M>` (a lookup through the enclosing lexical scopes) inside a routine did
  not find a package declared in its file: the routine's frame holds none of
  the file's packages. A `module`'s `sub EXPORT` that reads `::<Name>.WHO` saw
  an empty stash, so `use Name <A B>` died with "not exported".
