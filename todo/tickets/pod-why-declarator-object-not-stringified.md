# `.WHY` on a sub with leading/trailing Pod declarator comments doesn't stringify to the doc text

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Mu.rakudoc:435`).

## Repro

```raku
class Spell {};sub do-raw-magic(Spell) {};
#| Initiate a specified spell normally
sub cast(Spell $s) {
  do-raw-magic($s);
}
#= (do not use for class 7 spells)
say &cast.WHY;
# OUTPUT: «Initiate a specified spell normally␤(do not use for class 7 spells)␤»
```

- raku:
  ```
  Initiate a specified spell normally
  (do not use for class 7 spells)
  ```
- mutsu (`target/debug/mutsu`):
  ```
  Pod::Block::Declarator.new
  ```

## Analysis

`.WHY` on a routine returns a `Pod::Block::Declarator` object built from the `#|`/`#=` leading and
trailing declarator comments. `say`-ing it should stringify to the concatenated doc text (both the
leading `#|` block and the trailing `#=` block, newline-joined). mutsu returns a
`Pod::Block::Declarator` instance whose `.Str`/`.gist` isn't implemented — it falls back to the
generic `TypeName.new` gist instead of rendering the captured comment text.

## Affected files (starting point)

- Wherever `#|`/`#=` declarator comments are parsed and attached to a routine's `.WHY` (look for
  `Pod::Block::Declarator` construction in the parser/compiler).
- The `Pod::Block::Declarator` type's `.Str`/`.gist` method — needs to join the leading and
  trailing comment text the way the doc example expects.
