# `+@l` slurpy parameter's single-argument rule doesn't preserve list-context flattening semantics

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Parameter.rakudoc:197`).

## Repro

```raku
my @types is List = Mu, Any;
say -> *@l { @l }(@types)[0] =:= @types[0];        # OUTPUT: «False␤»
say -> +@l { @l }(@types)[0] =:= @types[0];        # OUTPUT: «False␤»
say -> +l { l }(@types)[0] =:= @types[0];          # OUTPUT: «True␤»
say -> *@l is raw { @l }(@types)[0] =:= @types[0]; # OUTPUT: «True␤»
```

- raku: `False`, `False`, `True`, `True`
- mutsu (`target/debug/mutsu`): `False`, `True`, `True`, `True`

Line 2 (`+@l`, the "single argument rule" slurpy sigil applied to an `@`-sigiled parameter)
diverges: raku still gives `False` (the element is a fresh, non-identical copy — same as plain
`*@l`), mutsu gives `True` (as if `+@l` behaved like the `is raw`/bare-`+l` forms that preserve
element identity).

## Analysis

Per `raku-doc/doc/Language/functions.rakudoc` / `signatures.rakudoc`, the `+` sigil-modifier
("single argument rule") only changes whether a single argument is treated as the whole list vs.
one element — it should not, by itself, change whether an `@`-sigiled slurpy's elements are
itemized copies or raw aliases. Only the sigilless (`+l`, no `@`) form and the explicit `is raw`
trait skip itemization. mutsu's `+@l` case appears to be conflating the `+`-sigil's "raw" binding
behavior with the sigilless-parameter's raw-alias behavior, treating `+@l` as if it implied `is
raw`.

## Affected files (starting point)

- Compiler/runtime code that binds the `+`-sigil-modifier slurpy parameter (`src/compiler/` or
  `src/runtime/` parameter-binding logic) — look for where `+`-sigil vs. `*`-sigil vs. bare
  sigilless slurpy binding decides whether to itemize/copy vs. alias each element.

## Suggested next step

Compare `--dump-ast` for the `+@l` and `*@l` closures to see whether they compile to the same
binding opcode/flag, then find where that flag incorrectly turns on raw/alias semantics for the
`+@l` case.
