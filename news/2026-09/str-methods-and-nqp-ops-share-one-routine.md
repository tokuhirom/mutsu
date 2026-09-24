# Str methods, nqp:: string ops and TRIR string ops now share one routine per primitive

An audit of the string layer found three independent implementations of every positional `Str`
primitive: the `Str` methods (grapheme-indexed, on the cached `grapheme_index`), the `nqp::` ops
(codepoint-indexed, on a single-slot `Vec<char>` memo in `nqp_char_cache.rs`) and TRIR's typed
string ops (codepoint-indexed again, on a four-slot `TrCharCache`). On top of that the `:i`/`:m`
adverbs had five separate `to_lowercase`-based folds, and infix `x` had four implementations.

They had drifted. Against rakudo, on `"ae\x[301]x\r\nY\x[1F1EF]\x[1F1F5]z"`, `nqp::chars` said 9
(rakudo 7), `nqp::index($s, "Y")` 5 (4), `nqp::ordat($s, 5)` 89 (127471), `nqp::eqat($s, "x\r", 2)`
1 (0), and `nqp::flip` turned `\r\n` into `\n\r`. `nqp::substr` clamped a negative start instead of
counting from the end. On the method side, `:i` lowercased instead of case-folding, so
`"STRASSE".starts-with("straße", :i)`, `"STRASSE".contains("ß", :i)` and `"straße".index("SS", :i)`
all disagreed with rakudo.

Following rakudo, where the method *is* the op, [ADR-0117](../../docs/adr/0117-str-methods-and-nqp-ops-share-one-routine.md)
makes `src/builtins/str_prim/` the single home: `chars`, `slice`/`nqp_substr`, `index`/`rindex`,
`eq_at`/`affix_matches`/`contains`, `char_at`/`nqp_ordat`, `find_char` (the cclass scans),
`graphemes`, `flip`, `concat`, `repeat`, `normalize` and the `Fold` used by every `:i`/`:m`. Every
layer -- `Str` method, `nqp::` op, the `~`/`x` opcodes and their reduction forms, TRIR's
`chars`/`substr`/`ordat`/`eqat` -- now calls it, and the two private memos are deleted. Positions are
graphemes everywhere, as in MoarVM.

Because the nqp layer now rides the per-payload grapheme index cache (eight slots, and no index at
all for flat ASCII), the O(n^2) cliff of #9129 -- a loop alternating two strings missed the
single-slot memo on every call -- is gone with it.

To keep it that way, `make check-str-prims` (a `make test` prerequisite and a CI step) fails the
build when an `nqp::` op table, the VM's nqp path or TRIR's runtime walks, cases, normalizes,
repeats or searches a string itself, and bans the old memo names anywhere in `src/`.
`t/vm/nqp-str-prim-parity.t` checks 44 rakudo-measured values, each row also asserting that the op
and the method agree.
