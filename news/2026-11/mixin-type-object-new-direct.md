# `.new` on a `Class but Role` type object goes straight to the constructor

A role-mixed type object (`C but R`, Monad's `parameterized(M, T).^pun but
Monad::Maybe::Some`) has no `Package` view, so none of the VM's construction
lanes claimed its `.new` and the call fell through the interpreter's whole
method cascade -- the by-name dispatch groups, mixin dispatch, an MRO walk
rebuilt per call (`classhow_mro_unhidden_names`) and finally the late
`Mu.new` row, which is `dispatch_new` on the receiver.

When no class of the base's MRO and no mixed-in role declares `new` or
`bless`, and no user `^find_method` exists, the call now makes that same
`dispatch_new` directly. `Monad::Maybe.some(1)`: 580k -> 540k instructions per
call (callgrind, profiling build, A/B against the merge base). Refs #17331.
