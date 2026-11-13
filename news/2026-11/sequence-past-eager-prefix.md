# A long finite `...` sequence keeps every term

A finite sequence with a built-in generator (arithmetic, geometric or string
successor) used to be cut off at 10000 generated terms: `1 ... 20000` had
10001 elements, `"a" ... "zzz"` stopped at `"ntq"` with 10001 of its 18278
terms. mutsu computes such a sequence's terms eagerly up to a prefix limit,
and a sequence still short of its endpoint at the limit was returned as that
truncated prefix. (Method::Protected's test builds its word list with
`"a" … "zzz"`.)

The sequence's generator is now data (`SeqGen`, `src/value/seq_gen.rs`), run
by one routine (`Interpreter::run_seq_gen`) for the eager prefix. A sequence
whose prefix runs out before its endpoint keeps the generator in its lazy
list, and pulling past the prefix resumes it. The sequence is still not
`.is-lazy`. A strict consumer (`.elems`, `my @a = ...`) drives it to its
endpoint, and a loop or `.head` over `1 ... 10**9` computes only the terms it
reads.
