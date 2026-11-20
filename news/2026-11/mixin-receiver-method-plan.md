# Method calls on an `Attribute` with a role mixed in are replayed from their call plan

The plain-method lane and the method-call plan (ADR-15737 D1) served only
instances of classes the program declared. AttrX::Mooish runs its hot methods
(`unbind-proxy`, `attr-var`, `store-with-cb`, ...) on an `Attribute` it mixes
its role into, `Attribute+{AttrX::Mooish::Attribute}`: a mixin type the
runtime synthesizes over a built-in base, which the lane refused because a
built-in class's dispatch probes may read the instance. `Attribute`'s only
probes in that stretch decide on the class and the method name, so a mixin
type over `Attribute` is now admitted, keyed by its registered name.

Three more shapes on the same calls now reach the plan:

- The late lane gate replays the plan as the early one does. A call whose
  arguments the early lane refuses (a `VarRef` for an `is raw` parameter)
  reaches only the late gate, which used to run the whole dispatch tail.
- An argument that is a shared cell (a caller variable an `is raw` parameter
  promoted, relayed onward) no longer rules the plan out. The lane key marks
  which positions hold a cell, so a plan decided for a cell is never replayed
  for a plain value of the same type.
- An `is raw` argument read from an attribute that already holds a shared cell
  (`$!attribute.unbind-proxy($!instance, $!val)` in Mooish's `AttrProxy`) is
  relayed on the fast binder; it used to need the general binder.

Separately, a generated accessor over an attribute bound to a `Proxy`
(Mooish's lazy attribute, `$inst.foo`) now FETCHes on the fast accessor path
instead of falling into the interpreter's method cascade.

A round of AttrX::Mooish `t/090-concurrent.custom`'s loop went from 6.17M to
6.00M Ir (-2.65%; callgrind slope R=120 to 360, profiling build). Per-round
CPU did not move outside the run spread (2000 rounds: 5.94 s vs 5.93 s, five
interleaved runs each). Measuring the slope from R=60 instead, as earlier
stages did, includes five JIT compilations that happen between rounds 60 and
120. Stage 4 of #18267.
