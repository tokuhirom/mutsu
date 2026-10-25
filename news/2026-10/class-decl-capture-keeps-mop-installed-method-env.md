# A class declaration no longer overwrites the environment of a method a closure installed through the MOP

`^add_multi_method` / `^add_method` hand the class a method literal together with the environment of
the closure that made it (`$attr`, `$name` of the attribute trait that built it). When the class
declaration ended, `capture_declared_method_envs` re-captured the declaring frame's scope for every
method of the class — the MOP-installed ones too — and replaced that environment. Whenever the
declaring file held a variable the closure's body also read (a file-scope `my $*FLAG`, Red's
`$*RED-INTERNAL`), the replacement existed and the closure ran with `$attr` and `$name` as Nil: Red's
`Post2.author` became `join(Post2, Nil, ...)`. Methods carrying the MOP-install marker are now left
alone. Red's `t/01-tdd` runs to the end (163 ok).
