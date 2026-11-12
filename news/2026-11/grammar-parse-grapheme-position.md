# Grammar parse positions count graphemes

`Grammar.parse` and `.subparse` now interpret `:pos` and `:c` in graphemes,
matching the positions reported by `Match.from` and `Match.to`. Cursor calls
made inside grammar methods continue to pass their internal code-point offsets
directly, including when the start rule has a wrapper.
The slang host converts parser positions before calling the public grammar
method, so slang productions after multi-code-point graphemes keep parsing.
