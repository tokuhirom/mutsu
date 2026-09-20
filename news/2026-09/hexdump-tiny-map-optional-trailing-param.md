# HexDump::Tiny goes green: a map block's optional trailing param no longer errors on a short chunk

`ecosystem-dist-roulette` drew `HexDump::Tiny` 0.6. Its one test file died at line 21 of the module
itself with `Not enough elements for map block arity`, even though rakudo passed all 4 assertions.

The module's `hexdump` implementation hex-encodes a `Blob`'s bytes two at a time:

```raku
@v.map(-> $a, $b? {
    $b ?? sprintf("%02x%02x", $a, $b)
       !! sprintf("%02x", $a)
}).join(" ")
```

`$b` is an *optional* trailing parameter — when `@v` has an odd number of elements, the block's
final call gets only `$a` and `$b` is left `Any`. mutsu instead raised an arity error on that last,
short chunk.

Reduced further: this only reproduced when the source was an Array **variable** (`my @v = (...)`),
not a List literal. Only an Array routes through the rw-capable map loop
(`eval_map_over_items_rw` in `src/runtime/resolution_map_grep_rw.rs`, since an Array's elements
support `is rw`/`$_` write-back); a List literal goes through the plain `eval_map_over_items`
sibling, which already handled optional trailing params correctly via its batch-computation loop.
`eval_map_over_items_rw`'s own "requires full binding" branch (taken whenever a param carries a
default, `?` marker, type constraint, etc.) reimplemented its own fixed-arity chunking loop instead
of reusing that batch logic, and unconditionally required a full-arity-sized chunk even when the
missing slot was optional.

Fixed by dropping the unconditional length check and clamping the final chunk to however many
elements remain; the normal call machinery already binds a missing optional/defaulted trailing
parameter to its default (and still raises "Too few positionals" for a genuinely mandatory one),
exactly as the List sibling's batch loop already relies on it to.

Pinned by `t/routines/signature/map-array-optional-trailing-param.t`. `HexDump::Tiny`'s ledger
record moves `red` (0/1 baseline files) → `green` (1/1, 4/4 assertions). No issues filed.
