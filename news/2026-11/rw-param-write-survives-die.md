# An `is rw` parameter keeps its write when the callee dies afterwards

`sub f(int $x is rw) { $x = 7; die }; my int $n = 1; try f($n); say $n` printed `1`
(rakudo: `7`). A natively compiled call copied the `is rw` spill slots back into the
caller's variables only when the body finished normally, and a generic call out of a
native body (a callee declared later in the file) read the argument containers back
only on success. Both now write the values back before the error propagates.

JSON::Fast depends on it: its `may-die-additional-content` does `try nom-ws($text,
$pos)` and relies on `$pos` having advanced before the end-of-comment `die`, so
`from-json('624/', :allow-jsonc)` and `'1 /* c'` were rejected where rakudo returns
`624` and `1`. JSON::Native t/04-json-fast.t now passes.
