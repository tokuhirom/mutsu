# A method body that calls a method on a computed value runs in the slot frame

The slot frame (ADR-18393) runs a planned method with no env overlay of its
own. Two things kept a body that makes a call out of it:

- its op list had no call;
- the plan asked for `gate.can_skip_merge`, which `cc.has_calls` always
  refuses.

So `multi method scale(Rat $r) { ($r * 4).Int }` ran in the lean frame. Its
frame costs about twice the slot frame's.

`slot_frame_body_ok` now admits `DerefContainer` and an argument-less,
unmodified `CallMethod`, a call on a stack value. Like an arithmetic op on an
object, such a call may run user code. That callee builds its own frame over
the caller's env, which the slot frame shares, and with no arguments no slot of
the method reaches it. The plan also takes `gate.merge_is_name_carry_only`: a
merge kept only to carry a callee's writes out of the method's overlay has
nothing to carry when there is no overlay, because those writes already land in
the shared env.

A call on a *variable* (`$s.chars`, `self.foo`, the `CallMethodMut` opcode)
stays refused. Its writeback reads and writes the receiver by name, which in a
slot frame would reach the caller's same-named variable. That is ADR-18393's
stage-2 readonly work, filed as #19266.

The `Rat` candidate's call drops from ~15.6k to ~13.3k instructions.
`bench-multi-dispatch` falls 1.1% on `scripts/bench-det.sh`, and no other
series gets worse. A new test pins what a callee does from inside such a
body: dynamic and outer-variable writes, exceptions, and `$CALLER::`.
