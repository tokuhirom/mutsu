# A regex split builds no separator Match it never reports

`bench-regex-split-subst` is one of the rows mutsu loses to Raku++. Its
deterministic run (`BENCH_DET=1`, callgrind, `MUTSU_JIT=off`) went from 400M to
231M instructions (-42%) through four cuts to the walk `.split(rx)` and
`.subst(rx, :g)` make over one subject:

- A plain `.split(/rx/)` built a full separator `Match` for every
  separator: capture reduction, the Match value, and then its drop. That was
  17% of the run, for an object only `:v`/`:kv`/`:p` ever report. The split
  now builds one only when the result reports it, and it does not attach the
  subject to the engine's captures either (one allocation per match).
- Each field was copied out of a per-call `Vec<char>` of the subject, one char
  at a time. It is now a byte slice of the invocant, located by a forward
  char-to-byte cursor (`runtime::char_byte_cursor`), with no second copy of
  the subject.
- Every separator looked the pattern up again: a scan for match-time scalars,
  a scan for interpolation and a parse-cache probe, 12% of the run. A static
  pattern is now parsed once per walk (`regex_walk_pattern`). A dynamic one
  still looks it up per match, because a code block in it may change what it
  interpolates.
- The native `.subst(rx, $str, :g)` built its result char by char from a
  second `Vec<char>`. It now copies byte slices too.

`t/regex/subst/regex-walk-split-subst-pieces.t` pins the pieces on multibyte
subjects, the separators `:v`/`:k`/`:p` still report, and `$/` after a global
subst.
