# `PROCESS::<&chdir>` is the process-level routine, and assigning to it dies

A `&` entry of the `PROCESS::` stash is the process-level dynamic `&*name`. mutsu
stored `PROCESS::<&foo> = ...` under an unrelated dynamic key, so the entry read
back as `Nil`, `&*foo` could not see it, and assigning to the setting's
`PROCESS::<&chdir>` was silently accepted.

The entries now live in the process stash under `&*name`, as rakudo keeps them:

```raku
say PROCESS::<&chdir>;                 # the process-level chdir
PROCESS::<&chdir> = sub ($p) { };      # dies: Cannot assign to an immutable value
PROCESS::<&log> = sub ($m) { note $m };
&*log('hello');                        # any frame, any thread
PROCESS::<&log> := sub ($m) { ... };   # binding still works
```

A bound entry (`:=`, or the installed `&chdir`) holds the code object itself, so
assigning to it dies; assigning to a fresh key gives it a Scalar that later
assignments store into (#9881).
