# `.Rat($epsilon)` is answered only where Rakudo declares it

`Int`, `Num`, `Rat`, `Complex` and the allomorphs take an epsilon in `.Rat` and `.FatRat`, and they already answered it from
their method-table rows. `Cool.Rat` and `Cool.FatRat` take none, but three name-matching arms of the one-argument cascade
still answered one: `"3.14159".Rat(0.01)` parsed and rationalized the string, and `(1, 2, 3).Rat(0.1)` answered `0`. Rakudo
refuses both, and so does mutsu now; the arms are gone (ADR-11276 §9.99).
