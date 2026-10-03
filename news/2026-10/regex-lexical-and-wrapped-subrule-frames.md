# `<&lexical>` calls and grammars with a wrapped token run on the compiled regex engine

Two more kinds of subrule call now run as frames of the compiled regex engine instead of going
through the tree walk's eager producer (ADR-0135 Slice E, eleventh part; #7548):

- `<&r>` where `r` is a lexical Regex. Its defining scope is installed while it matches, and
  installed again whenever backtracking re-enters it.
- Any call in a program where some token was `.wrap`ped. Until now, a single wrap anywhere sent every
  later call of every grammar to the walk. Now only a call of the wrapped token itself does. The
  routine frame that lets a wrapper find its calling rule in a Backtrace is pushed per call frame.

A `{ … }` block inside such a callee now runs only at the end the match actually enters, as in
rakudo, not once for every end the walk computed. In `my regex lr { a+ { @log.push: $/.to } }`
matched by `/^ <&lr> 'x'/` against `aax`, the block now runs once, not twice.
