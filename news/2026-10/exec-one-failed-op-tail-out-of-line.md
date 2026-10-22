# The per-op wrapper keeps its failed-op tail out of line

#14189 put a `match` on the failed-op result into `Interpreter::exec_one`, which runs once per
executed op. It was inlined into every call and `bench-mandelbrot` / `num-arith` paid ~9% more
instructions (the issue blamed #14190's GrammarHOW check; an A/B build of the commits before and
after it showed #14190 changes nothing and #14189 carries the whole step). The tail is now the
`#[cold]` `exec_one_failed`, so the success path is a probe and a return again (#14287).
