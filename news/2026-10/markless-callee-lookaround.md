# A `:m` callee with a look-around runs as a markless frame

A whole-`:m` rule called as `<r>` over a subject that has combining marks runs
as a markless frame of the compiled engine: each atom compares the base of the
grapheme it stands on and consumes the whole grapheme, so the callee enters its
ends one at a time, as rakudo does. A look-around in the callee used to make it
decline that compile and fall back to the eager `:m` path, which ran a code
block once per end computed (`2,1` where rakudo logs `2`).

Now the look-around's body runs as a markless nested run of its own over the
same subject: it compares bases too, and a lookbehind tries only start
positions on a grapheme boundary. A body that cannot compile markless still
declines the whole callee (#13176).
