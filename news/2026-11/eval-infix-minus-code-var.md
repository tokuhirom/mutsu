# `EVAL '&infix:<->'` no longer reports `Undeclared routine`

The guard that refuses an undeclared word-shaped infix (`&infix:<not-an-op>`) in `EVAL`
accepted any name made of lowercase letters and `-`, so the symbolic `-` qualified as a
"word" and `&infix:<->` threw `Undeclared routine`. It now requires at least one
letter. This unblocks three `t/` files for the `EVAL($ast)` ratchet.
