# `callsame` / `nextsame` / `callwith` inside a user `method bless` reach `Mu.bless`

`class C { has $.x; method bless(*%_) { callsame } }; C.bless(x => 1).x` answered Nil: the
user `bless` had no native base candidate, so the deferral chain ended empty. A user `bless` is
now treated like a user `new` when the method dispatch frame is built (`new_base_override`),
and `native_mu_base` answers its `Mu` base with `dispatch_bless`, so `callsame`, `nextsame`,
`nextwith` and `callwith` (with changed arguments too) construct the object as Rakudo does
(#16335). `Mu.new` still builds through the native constructor without asking the user `bless`;
that part is tracked with the ValueTypeCache work (#16103).
