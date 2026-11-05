# A `.map`/`.grep` call decides which captured names it can drop once per name

Each `@a.map(...)` / `.grep(...)` Seq builds its own inline-loop plan, and the
plan asks `droppable_subject` about every name in the callback's captured env
(a type, enum or constant name the body can only reach by naming it). With a
module loaded the capture holds ~60 such program names, and each ask parsed the
name's text and interned its subject, every time a Seq was built.
`droppable_subject` is a pure function of the symbol, so it is now a
per-thread memo.

Measured with `use Cro::HTTP2::RequestParser` loaded and
`for ^$n { $t += @a.map(* * 2).grep(* > 4).elems }` (profiling build, loop-only
callgrind, slope 100 -> 300 outer iterations, same binary path for both sides):
680,165 -> 619,211 Ir per outer iteration (-9.0%).
