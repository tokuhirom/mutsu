# A BEGIN in a nested block sees that block as `$*R.current-scope`

At BEGIN time, `$*R.current-scope` is the scope the phaser stands in. For a
BEGIN at the unit's top level that is the compunit. For one inside a nested
block, a routine body or a package body, it is that block. CompUnit::Util keys
its `set-lexpad` / `get-lexpad` installs by that object, so a lookup in a
nested block must not see the unit's entries.

mutsu answered the compunit for every BEGIN. A BEGIN nested in a block is
moved into the unit's prologue to run at compile time (ADR-0134), and that move
lost the block it came from.

The prologue walk now gives each block an id, and every BEGIN of the block
carries it, whether it is moved or not. The phaser's `CheckPhaserStart` passes
the id on, and `$*R.current-scope` answers one block-scope object per id. A
`resolve-lexical-constant(:current-scope-only)` from such a BEGIN no longer
reaches the unit's names.

CompUnit::Util 0.7.0's `t/08-get.rakutest` passes 5/5 (#17515, #15371).
