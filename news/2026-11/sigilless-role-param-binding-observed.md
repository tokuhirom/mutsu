# A sigilless role parameter is visible to the role's methods

`role R [Mu:U \t] { method foo { t } }` answered the bare word `t` instead of
the bound type when the role was composed into a class or mixed into a class
instance (`(Box.new but R[Int]).foo`, `class K does R[Num]`); only a value
mixin (`1 but R[Int]`) worked (#18058). Red mixes `CMModel[model]` into a
result set this way.

A role method skips installing its composition bindings when its body names
none of them (#17331). A sigilless parameter is bound under its term key
(`\t`, ADR-12441) while the body names it by its spelling (`t`), so the scan
judged the binding unobservable and the method ran without it. The scan now
also matches a term key's spelling.
