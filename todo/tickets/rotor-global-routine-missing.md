# Global `rotor()` routine (v6.e.PREVIEW) is not implemented

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/List.rakudoc:1207`).

## Root cause

`List.rotor(...)` (the **method**) already works correctly in mutsu. The doc example uses
`rotor(...)` as a **global routine** (gated behind `use v6.e.PREVIEW;`), which raku
provides but mutsu does not — even with the pragma present, mutsu has no top-level
`rotor` sub registered:

```raku
use v6.e.PREVIEW;
say rotor(3, 'a'..'h').join('|');  # raku: a b c|d e f
                                    # mutsu: Unknown function: rotor
```

Without the pragma, raku itself also fails ("Undeclared routine: rotor"), confirming this
is specifically a `v6.e.PREVIEW`-gated global, not a base-language gap. Lower priority
than the other doc-diff findings from this sweep since it depends on preview-language-
version support mutsu may not otherwise track.

## Minimal repro

```raku
use v6.e.PREVIEW;
say rotor(3, 'a'..'h').join('|');
```

## Affected files (starting point)

Wherever global routines are registered (`runtime/builtins_*.rs`) and wherever
`v6.e.PREVIEW`/language-version pragmas gate feature availability — needs a global
`rotor(...)` sub that simply delegates to the existing `List.rotor` method
implementation, registered only when the preview pragma is active (or unconditionally, if
mutsu doesn't otherwise gate preview features).
