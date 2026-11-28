# A TWEAK with attributive parameters runs in the lean method frame

`submethod TWEAK(:%!meta, :@!resources --> Nil) { ... }` ran on every
construction through the general fast binder, which asks on each call about
every feature a method might use: deprecation, `callframe`, `samewith`, role
bindings, the method's capture, `state`, attribute defaults, the return type,
every parameter shape. For a phase step those answers are a function of the
submethod and its class.

A construction plan now records, for a pinned BUILD/TWEAK whose signature is
the invocant, the implicit `*%_` and optional untyped attributive named
parameters, that it runs in the lean frame. The lean frame learned the shapes
it needed for that (`LeanShapes::PhaseStep`): attributive named parameters,
bound by the one named-parameter arm the fast binders now share
(`bind_named_param_fast`), and a body that creates closures, for which it roots
the frame's callable id as the general binder does. The fast-path gate's
verdict is replayed from the signature plus four per-call checks, and debug
builds assert it against the real gate on every lean call.

The bind-only phase step (#19437) now calls the same named-parameter arm
instead of a copy of it.
