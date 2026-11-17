# `my Int $x` keeps its per-site memo after a `::T` capture was bound

A typed declaration of a plain builtin type (`my Int $x`, `my Str $s`) answers
from a per-site memo that its constraint resolves to itself. The memo was
switched off by the process-global "a `::T` capture was ever bound" latch, so
once any routine of the program -- or of one of its dependencies -- bound a
type capture, every typed declaration everywhere went through the full
resolution (`try_resolved_type_capture_name`, `unshadowed_builtin_type_name`,
...).

The memo now asks the question the capture step actually answers: is a capture
bound under this constraint's own name? That is `has_type_capture_binding`,
the per-name filter word plus one env probe when a capture of a name with the
same filter bit was ever bound. `sub g(::Str $x) { my Str $y = $x }` still
substitutes the captured type.

Callgrind, profiling build, after one `::T` binding: a loop of three typed
declarations went from 34.2k to 27.4k instructions per iteration (-20%). On
App::Moneymoor's property soak (a reduced copy of
`t/15-invariants-property.rakutest`) the program total moved only ~1%: most of
the remaining per-declaration cost is the type-object seed and the
declaration's own ops. Refs #17265.
