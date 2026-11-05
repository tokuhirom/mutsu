# An `augment`ed multi candidate does not join an already-composed subclass's dispatcher

Rakudo derives a subclass's multi dispatcher from its parent's when the subclass is composed,
so `augment class A { multi method m(Str $x) {...} }` leaves `B.new.m("s")` on the candidates
`B` (a subclass with its own `multi method m`) saw before. mutsu resolved the candidate set from
the live MRO and saw the new candidate.

Each candidate an `augment` adds carries the augment's declaration-order stamp
(`MethodDef::augment_epoch`), and the augment records on every user subclass that declares its
own `multi` candidates of that name the stamp from which candidates of its ancestors no longer
join (`Registry::frozen_dispatchers`). The candidate walks (`resolve_method_with_owner_impl`,
`resolve_sequence`) skip a hidden candidate; a subclass without its own candidates, the augmented
class itself and a single-dispatch method keep seeing it (#16049).
