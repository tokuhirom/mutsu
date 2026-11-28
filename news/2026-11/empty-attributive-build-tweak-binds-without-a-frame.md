# An empty attributive BUILD/TWEAK binds without a method frame

`submethod BUILD(:$!a, :@!b) { }` and the same `TWEAK` are the classic way to
let a constructor fill private attributes. Such a submethod's whole effect is
its signature: the body has no ops. Each construction still ran it as a full
method call: a call frame, an env overlay, the routine and method-class frames,
the parameter list, the result finalize.

A construction phase step now notices this shape once, when the class's
constructor plan is built, and binds the attributive parameters directly into
the object under construction. The bind is the fast binder's own named arm,
taken only when the fast-path gate admits the call, so a typed or defaulted
parameter, a `Mu` argument or alias metadata still runs the frame; a wrapped
submethod still runs its wrapper.

On `bench-ctor` (`Spec::TWEAK(:$!spec) { }`, callgrind, per construction):
220.4k → 211.7k instructions, 135 → 131 allocations (#19399).
