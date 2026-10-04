# Successor and predecessor methods use handler rows

The built-in method table now owns `succ` and `pred` for `Str`, `Int`, `Num`, `Rat`, `FatRat` and `Complex`. Their handlers call the shared successor and predecessor routines, and the remaining cascade cases use those same handlers. A callgrind loop alternating numeric successor, numeric predecessor and string successor used 68.0% fewer instructions.
