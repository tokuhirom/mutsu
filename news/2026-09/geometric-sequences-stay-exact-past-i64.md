# Geometric sequences stay exact past i64

Exact geometric sequences now promote integer products to arbitrary precision instead of converting them to `Num` on overflow. Finite sequences also compare large elements with large endpoints exactly, so `.tail` stops at the right term. The deferred generator shares the same exact rational multiplication routine.
