# Fix `@0` (numbered match-capture array variable) failing to parse inside a `[...]` array literal

`@0`, `@1`, ... — the array-context view of the positional match captures
`$0`, `$1`, ... — failed to parse with `Confused: Two terms in a row`
whenever it appeared as an element inside a `[...]` array literal (`[ @0 ]`,
`[ @0».Int ]`). Standalone uses (`say @0».Int`) appeared to parse, but only
by accident: a digit was not accepted as a valid start character for an
array-sigil variable name, so `array_var()`'s bare-`@` (anonymous array)
branch matched instead, silently leaving the digit unconsumed as a stray
trailing token. At statement level that stray digit just became its own
(harmless, if slightly wrong) sunk expression statement; inside a `[...]`
element list, with no statement boundary to absorb it, it produced a hard
parse error instead.

Fixed in `src/parser/primary/var/sigil_vars.rs`'s `array_var()`: a leading
digit run after `@` is now parsed as a numbered positional-capture array,
normalized the same way `$0`'s scalar form is (`@00` collapses to `@0`), and
lowered to `$N.list` — reusing the already-correct `$N` resolution and the
same `.list`-lifting idiom the parser already uses for `@$<name>` /
`@$/`-style scalar-to-array coercions, rather than adding a second,
independent resolution path. Verified element-by-element against a live
`raku` reference (`[ @0 ]`, `[ @0.Int ]`, `[@0]`, `[ @0».Int ]`, and the
repeated-capture case `"a1b2c3" ~~ / (\d)+ /`) — all match exactly. Pinned in
`t/numbered-capture-array-literal.t`.

## Discovered while

Investigating what blocks `CSV::Table` (a candidate for the batteries CSV
slot, see `docs/batteries/csv.md`) on mutsu: its dependency `Text::Utils`
unconditionally pulls in `Font::AFM` (PDF font-metrics text-width
calculations, unrelated to CSV), whose `Font::AFM.rakumod:436` has
`my Array $bbox = [ @0».Int ];`.

## Residue

With this fix, `use Font::AFM` parses further than before but hits a new,
unrelated blocker: `method dispatch:<.?>(...)` (custom dynamic-dispatch
method syntax) is not a recognized method-name category. Filed as
`todo/tickets/method-dispatch-colon-question-syntax.md`. `CSV::Table` is
still blocked; the next person picking this up should start there.
