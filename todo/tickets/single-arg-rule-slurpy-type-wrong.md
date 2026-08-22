# Single-argument-rule slurpy parameter (`+name`) yields `Array`, not `List`/`Seq`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/signatures.rakudoc:849`).

## Repro

```raku
sub zipi( +zape ) {
    zape.^name => zape
};
say zipi( "Hey " );  # raku: List => (Hey )     mutsu: Array => [Hey ]
say zipi( 1...* );   # raku: Seq => (...)        mutsu: Array => [(...)]
```

- raku: the single-argument rule (`+name`) binds a single non-list argument as a 1-element
  `List`, and passes an already-list-like argument (e.g. a `Seq`) through unchanged (its
  original type, here `Seq`).
- mutsu: always produces a plain `Array` regardless of the argument's actual/original type.

## Root cause hypothesis

mutsu's `+name` slurpy parameter handling likely reuses the same "collect into an `Array`"
logic as the ordinary `*@name` slurpy, without implementing the single-argument-rule's actual
semantics: wrap a lone non-list scalar argument in a `List` (not `Array`), and pass an
already-List/Seq-shaped single argument through by reference/identity (preserving its dynamic
type) rather than re-collecting it into a fresh container.

## Affected files (starting point)

- Signature-binding code that recognizes `+name` slurpy parameters (grep for `"+"` sigil
  handling alongside `*@`/`*%` slurpy binding in `src/compiler/` or `src/runtime/` signature
  binding) — needs to special-case single-argument-rule collection instead of routing through
  the generic array-slurpy path.
