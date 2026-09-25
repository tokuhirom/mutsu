# A trailing comma after a listop's block argument ends the argument list

`DB::Migration::Declare`'s `t/model-check.rakutest` passes blocks like

```raku
throws-like
        {
            check {
                migration 'Setup', { ... }
            },
        },
        X::DB::Migration::Declare::MigrationProblem, ...;
```

where the inner block's last statement is `check { ... },` — the imported
`check` called with one block argument and a trailing comma. mutsu failed with
this #7988 cluster's generic `Confused. expected statement: ...` message (reported
at the `},` line).

The bareword-plus-block branch of the call parser (`identifier_call.rs`) treated
the comma after `{ block }` as the start of further arguments and required an
expression there, so the closing `}` was an error. A trailing comma after other
listop arguments (`foo 1, 2,`) was already accepted. The block branch now ends
the argument list when the comma is followed by `;`, `}`, `)`, `]` or the end
of input, and calls the listop with just the block, as rakudo does
(`sub foo(&b) { b() }; my @x = foo { 1 },` is `[1]`).

Pinned by `t/collections/listop-block-arg-trailing-comma.t`. Every
DB::Migration::Declare test file now matches rakudo.
