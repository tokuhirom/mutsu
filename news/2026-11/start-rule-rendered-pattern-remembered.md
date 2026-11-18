# A literal start rule's rendered pattern is remembered

`Grammar.parse` resolves its start rule through `eval_token_def`, which
evaluates the rule's body in a frame of its own — an `env` clone, the
declaring scope's captures, a readonly frame, a routine frame — and then
renders the pattern, interpolating and baking the rule's bound parameters.
For a rule with no parameters, called with no arguments, whose body is one
regex literal, every step is decided by the literal alone: the body evaluates
to it, and both rendering passes touch only bound parameters (of which there
are none). That work was repeated on every parse.

The rendered pattern is now remembered per thread under the instantiated
literal (`runtime/token_render_memo.rs`), and such a rule skips the frame and
the render on every later parse. `G.parse('xy')` with three tokens went from
138.8k to 115.9k instructions per parse (callgrind slope, profiling build), and
`Cro::Uri.add('/page/N')`, which parses twice per call, from 2.740M to 2.668M.
