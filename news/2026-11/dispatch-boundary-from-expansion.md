# A method with one deferral candidate skips the candidate match

When a program can defer (`callsame` and friends appear anywhere), every
method call builds a dispatch frame. If the method has nothing to defer to,
the frame is an empty boundary, and `build_method_dispatch_frame` takes that
fast path when the structural candidate count says there is at most one
candidate.

That count includes a role method composed into a mixin twice, once at each
level: `Attribute+{AttrX::Mooish::Attribute}`, every AttrX::Mooish attribute.
So every call of such a method matched and cloned the whole deferral
expansion, only to find the single candidate it already had. The deferral
expansion bounds every list the build makes. So when the expansion has at
most one entry and the winner is already resolved, the build now returns the
same empty boundary before the per-call match.

On AttrX::Mooish's `t/090-concurrent.custom` probe (callgrind slope,
R=120→360, profiling build) this is 4,978,958 → 4,940,180 instructions per
round (−0.8%). Web `html-to-text` is unchanged. ADR-18244 stage B (#18274).

Found on the way, pre-existing: #18363. A `callsame` in a mixed-in role
method consumes the enclosing method's deferral candidates.
