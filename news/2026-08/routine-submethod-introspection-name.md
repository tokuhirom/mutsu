# `&?ROUTINE.^name` inside a `submethod` reports `Method` instead of `Submethod`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Routine.rakudoc:29`).

## Repro

```raku
class Foo {
    submethod bar { &?ROUTINE.^name }
};
say Foo.bar;
```

- raku: `Submethod`
- mutsu: `Method`

## Root cause hypothesis

`&?ROUTINE` resolves to the currently-executing routine's `Code`-family object, and `.^name`
reports its dynamic type. mutsu's `submethod` declarations appear to construct (or tag)
`&?ROUTINE`'s underlying value as a plain `Method` rather than as a `Submethod`, so the
metaclass name comes back wrong even though a `submethod` otherwise dispatches and behaves
correctly (it's the introspected type identity that's off, not the call itself).

## Affected files (starting point)

- Wherever `&?ROUTINE` is bound in the routine's compiled prologue (search for `ROUTINE` in
  `compiler/`/`vm/vm_register_ops.rs`) — check whether it always tags the closure/routine value
  as `Method`, or whether `submethod` registration itself doesn't record a distinct
  `Submethod`-typed routine object for `.^name` to report.
- `runtime/class.rs` — submethod registration, to compare against how `Method` is tagged for a
  regular `method`.
