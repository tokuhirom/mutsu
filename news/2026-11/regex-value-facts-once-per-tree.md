# A regex value is analysed once per source tree, and keeps its tree in the module cache

`use Readline` spends most of a warm load in its `LIBREADLINE` sub, which lists
`/lib/x86_64-linux-gnu` with `dir(:test(rx/:i libreadline\.so\.(\d+) $/))` once
for each of ~280 `is native` traits: ~383,000 regex matches per load (#18003).
Two things made each of those matches slower than it needed to be.

**The regex had lost its source tree.** The compiled module cache serialized a
plain regex value as its pattern text alone (and an adverbed one without its
tree). A regex from a precompiled module therefore took the string path on
every match: the pattern text was scanned and looked up again each time, about
13,000 instructions per match, and a warm load matched through a different path
than a cold load of the same module. The cache now keeps the tree
(`SerValue::RegexSourced`, and a `source_tree` field on `RegexWithAdverbs`).

**Every match re-derived the same facts.** With a tree, a match still decided
on every call whether the pattern needs the string path, which variables it
interpolates, and which execution plan it lowers to, and it found that plan by
hashing the whole tree and then the pattern text. All of those depend only on
the tree and the pattern, and the plan also on the package and the token
generation, which is exactly the key of the cache it was looked up in. They
are now decided once per tree and kept in a per-thread table keyed by the
tree's address (`runtime/regex/regex_value_memo.rs`). The entry holds a `Weak`
to the tree, so the address cannot be reused while it exists, and the table
starts over past 4,096 trees. (A field on `RegexTree` itself would have widened
`Expr`, which holds a tree inline and is kept at 120 bytes.)

Measured:

- A loop of 2,000 `$s ~~ rx/:i libreadline\.so\.(\d+) $/` matches: 41.9k → 30.5k
  instructions per iteration (callgrind, profiling build, 2,000 vs 4,000
  iterations).
- Warm `use Readline` (callgrind, release build): 15.30 G → 12.45 G
  instructions (-18.6%). Wall clock, three interleaved rounds of two runs:
  2.66-2.91 s before, 2.38-2.53 s after (rakudo: 0.40 s).

The rest of the gap is the 280 trait applications that run again on every warm
load, which is ADR-18012's precompilation work.
