# `cas` on a shadowing object-valued scalar keeps its own binding

A `cas` on an inner `my $y` holding an object (`Instance`, `Array`, `Hash`, ...) fell back to
the name-keyed legacy atomic lane, which is shared by every binding spelled `y`: the swap
reached the shadowed outer variable, and the outer variable's own earlier swap was lost.

When the compiler resolved the call site to a slot of the running frame, the atomic helpers
now box such a value into that binding's own cell (only Proxy and the lazy sequence kinds
stay unboxed), so the binding identity #12006 introduced for plain values also holds for
objects. Regression test: `t/concurrency/thread-lock/atomicint-cas-object-shadowed-by-inner-declaration.t`.
