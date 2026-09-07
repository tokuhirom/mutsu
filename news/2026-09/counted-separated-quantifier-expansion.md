# A counted separated quantifier expands greedily, and without a capture

```raku
my $c = 0;
say "a,b,c" ~~ / [ \w+ { $c++ } ] ** 1..3 % ',' /;   # raku: ｢a,b,c｣   mutsu: ｢a｣
say $c;                                              # raku: 3        mutsu: 1
```

The ticket narrowed it to needing all three of a **non-capturing** group, a
**code block** inside it, and a **counted** (`** N..M`) separated quantifier —
swap any one and the chain extended correctly. That combination turns out to
select one particular code path, and the block only makes an existing defect in
it visible.

## Root cause

A separated quantifier whose atom (and separator) carry **no capture** is not
matched by the native `match_separated_quantifier` path at all: the parser
string-expands it (`expand_ltm_pattern` → `build_ltm_expansion_inner`), because
the native path is reserved for capture-bearing atoms whose per-iteration
capture structure must survive. `** N..M % sep` expanded to an alternation of
one branch per repetition count — and built them **ascending**:

```
[ \w+ ] ** 1..3 % ','   ->   ( A | A,A | A,A,A )
```

`|` is LTM, so with a purely declarative atom the longest branch wins whatever
the order. An atom carrying a code block **ends the declarative prefix**
(ADR-0009): every branch then measures the same prefix, the tie breaks by
order, and the shortest branch won. Hence "swap any one and it works" — remove
the block and LTM ranks properly; make the group capturing and the native
(greedy) path takes over; drop the `N..M` and there is no alternation to
mis-order.

The alternatives are now built `.rev()`, longest first — which is what a greedy
quantifier means, and what the sibling no-separator arm right above already
did. The zero-count branch moves with it: `build_exact_list(0)` is the empty
string, and descending order would put it LAST, making `[AA|A|]` — a trailing
empty branch, which is misdetected as a null regex (caught by
`roast/S05-metasyntax/proto-token-ltm.t`). A `**0..max % sep` now builds
`1..=max` and wraps the group in `[...]?`, again exactly as the no-separator arm
already did.

## A second defect in the same three lines

The alternation was wrapped in `(...)`, a **positional capture**:

```raku
("a,b,c" ~~ / [ \w+ ] ** 1..3 % ',' /).list.elems   # raku: 0   mutsu: 1
```

A `**N..M % sep` over a non-capturing atom must introduce no capture. It is
`[...]` now — the same fix, and the same comment, the no-separator arm above
already carries. Capture-bearing atoms never reach this expansion, so nothing
loses a capture by it.

## Scope

`t/regex-lazy-candidate-enumeration.t`'s F6/F6b rows lose their `todo` markers,
and eight rows (H1-H8) are added: the longest chain for `** 2..3`, the absent
positional capture, both of those with a block, a chain shorter than the max, a
single element, the minimum still being enforced, and `%%` taking a trailing
separator. The whole file (80 assertions) passes under `raku` unchanged, and
all 97 whitelisted `roast/S05-*` files (5614 subtests) are green.
