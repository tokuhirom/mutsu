# A chained element store through an undefined captured scalar is kept

`my $nest; my sub deep { $nest<a><b> = 1 }` left `$nest` undefined: the
two-level chain vivified the root in the frame's env, but the walk that follows
goes through the root the descent resolves (a file-scope lexical's unit slot or a
capture cell), which stayed undefined. The root is now vivified there too, as
the same node. A three-level chain also holds its container in the scalar
(`$n<a><b><c> = 1` is `${:a(...)}`, not `{:a(...)}`).
