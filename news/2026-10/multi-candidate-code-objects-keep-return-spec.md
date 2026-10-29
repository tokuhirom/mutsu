# A multi candidate's code object keeps its return spec

The code objects built for a multi family's candidates (`&f.candidates`, and
the dispatcher an import hands the caller) did not carry the candidate's
return spec, so an imported `multi sub seek(... --> True)` answered `Nil`
instead of `True` and `--> Int` was never enforced. Each candidate now carries
its own `__mutsu_return_type`, and never the capturing frame's. This makes
P5seek's `t/01-basic.t` pass (11/11).
