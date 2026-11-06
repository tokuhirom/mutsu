# `:exists` on a positional subscript of a Seq

`(1, 2, 3).Seq[1]:exists` and `$seq[0]:exists` answered `False`; a finite Seq now answers from
its cache, like its plain `[ ]` read. Found with Iter::Able's `fill-undef` and a Seq filler (#16939).
