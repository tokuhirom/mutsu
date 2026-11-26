# `Str.split` stops copying each piece and separator

A string-separator `.split` allocated about 4.6 times per piece (#19201):

- it built a `Vec<char>` of the whole invocant on every call, although only the
  empty separator uses one;
- it copied the separator into every match;
- it copied each piece once when cutting it out and again when turning it into
  a `Str`.

Now the char vector is built only for the empty separator. Each separator's
`:v` value is built once per call and shared by its matches. Each piece's
buffer moves into its `Str`. The result vector is pre-sized. The regex split
paths also stop collecting a separator text that only the `Match` object is
ever reported for.

Measured with callgrind (`MUTSU_JIT=off`): a 20-piece `split(' ')` loop goes
from ~4.6 to ~2.7 allocations per piece. `benchmarks/word-count.raku` makes
26% fewer `malloc` calls (727,855 → 537,862) and executes 8% fewer
instructions (850.4M → 782.6M). The rest of #19201 is sharing the invocant's
buffer between pieces instead of copying each one.
