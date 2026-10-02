# A `Str:D`-style type object term works inside EVAL

`say Str:D` printed `(Str:D)`, but the same term inside `EVAL q[say Str:D]` died with
`Undeclared name: Str:D used at line 1`. The `Int:U` and `K:D` spellings failed the same way,
including for a class `K` declared in the snippet itself. A `throws-like 'm(Str:D)', ...` (which
EVALs its code string) therefore failed before reaching the call it tested.

EVAL's undeclared-name check (`check_eval_undeclared_names`, `src/runtime/eval_name_scans.rs`)
looked the whole bareword up, smiley included, and no registry knows `Str:D` under that name.
A smiley term is now known exactly when its base type is. An undeclared base (`Nope:D`) is still
reported (#10814).
