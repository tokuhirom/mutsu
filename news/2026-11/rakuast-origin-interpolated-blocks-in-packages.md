# `.origin` of terms in interpolated blocks inside package bodies

The view converts a copy of a package body's code. A copied term therefore
finds its range by its shape within the statement being converted. A string
with several interpolated blocks, such as `"{$s.from}-{$s.to}"`, holds several
terms of the same shape (`$s`), so none of them could be told apart, and the
variables, calls and names inside the blocks were left without ranges.

The parser now notes where each interpolated block is written. The view makes
that block the statement its terms belong to, so each `$s` finds the one range
inside its own block, as on rakudo 2026.11 (#15780).
