# A sigil-less `our constant` is listed by its bare name in the package stash

`OurGlobalsBase::.keys` listed `our constant answer = 42` as `$answer`, because the package store
spells a term and a scalar of the same package the same way. The store now records which
qualified keys hold a sigil-less `our constant`, and the stash enumeration (the `our` store walk and
the env mirror) reports them as `answer`, as Rakudo does (#11877). An `our $x` keeps its `$`.
