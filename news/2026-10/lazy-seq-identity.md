# `===` and `.WHICH` on an infinite lazy Seq

`my $s := 1...*; say $s === $s` died with "Cannot coerce an infinite lazy list to a strict list"
because the binary-operator wrapper forced a `LazyList` operand before `===` could compare it, and
`.WHICH` of a lazy list answered a fresh id on every call. `===` / `!===` now compare a lazy list by
its own identity without forcing it, and `.WHICH` is stable (`Seq|<object>`). Closes #12972.
