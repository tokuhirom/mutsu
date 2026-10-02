# A Seq that fails a typed `@` parameter is reported as a Seq

`sub h(Int @a) {}; h((1,2).Seq)` used to die with "expected Positional[Int] but
got List ((1, 2))": the binder rebinds a Seq (or a gather's lazy list) argument
to a List view before the typed-container check, and the failure was built
from that view. The binder now keeps the caller's argument and reports it, so
the message reads "but got Seq ((1, 2).Seq)" and `.got` is the Seq, as in
Rakudo (#10921).

The type-check message's `(repr)` also reifies a not-yet-run `.map`/`.grep`
Seq first (ADR-0058), so `h((1..3).map(* + 1))` names `((2, 3, 4).Seq)` rather
than the empty seed `(().Seq)`.
