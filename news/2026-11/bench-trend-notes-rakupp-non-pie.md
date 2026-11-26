# The bench trend notes that rakupp's startup lead is mostly its non-PIE link

rakupp is a non-PIE executable: `EXEC`, loaded at `0x400000`, no relative
relocations. mutsu is a PIE, so before `main` the dynamic loader applies ~80k
relocations and copy-on-write faults in ~400 pages of `.data.rel.ro`.

Linking the same mutsu source non-PIE (`-C relocation-model=static` on the
binary) takes an empty program from 1,086 to 697 minor faults and from 1.29x to
1.10x rakupp. `bench-startup` goes from 1.43x to 1.10x and `string-concat` from
1.28x to 1.13x. Each figure is the median of 21 interleaved runs. The grammar rows
barely move: their gap is first-parse work, not startup.

The dashboard's **ratio vs rakupp** cards for `bench-startup` and
`string-concat` now carry that note, as the JSON::Fast cards do for rakupp's
native JSON parser. The numbers are in `docs/rakupp-gap-survey.md`. Linking
non-PIE trades ASLR for the binary's own code and is the maintainer's call
(#11158).
