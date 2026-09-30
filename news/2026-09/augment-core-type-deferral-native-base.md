# Deferring out of a method augmented onto a core type reaches the builtin

A method added to a core type with `augment` that defers with `nextsame`,
`callsame`, `nextwith` or `callwith` now reaches the builtin method of the
receiver's type as its final candidate, as the setting's own method is in
Rakudo's MRO:

```raku
use MONKEY-TYPING;
augment class Str   { multi method FatRat(Str:D:) { nextsame } }
augment class Array { method sort(|c) { "sorted:" ~ callsame().join(",") } }
say "1.5".FatRat.raku;  # FatRat.new(3, 2)  (was Nil)
say [3, 1, 2].sort;     # sorted:1,2,3      (was "sorted:" plus a Nil warning)
```

mutsu implements core methods natively, so they are not `MethodDef`s and the
user MRO walk ended at the augmentation, answering `Nil`. The exhausted
deferral now calls the builtin through a scoped bypass (`native_base_bypass`)
that hides the augmentation from the "did user code override this?" gates for
exactly that receiver and method while the builtin runs, so it cannot re-enter
the augmentation while other receivers still reach it. This was the last
non-parity assertion of `FatRatStr`'s `t/03-makestr.rakutest` (#10198).
