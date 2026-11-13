# Pool threads share their site memos; cheaper iterator calls, `int` enums and `uint` steps

Re-measuring ParaSeq's suite next to rakudo (every file now matches rakudo's results) left the
speed gap of #17337, and four causes of it turned out to be general:

- **Site memos across threads.** The memos kept in compiled code that every thread runs (bareword
  type objects, class operands, the method-table call-site lane, lexical-import sites) are keyed on
  the registry write generation, and each interpreter counted that in a range of its own. Two pool
  workers running one site therefore overwrote each other's memo and re-resolved it on almost
  every call: a constructor called from three tasks took twice as long as from one. A generation
  value now names one registry content (one process-wide sequence; a fork starts at its parent's
  value), so threads that share their declarations share the memos (ADR-17337 D4).
- **The Iterator protocol.** `$it.pull-one` on a built-in iterator ran the whole `CallMethodMut`
  probe chain before reaching the protocol row, ~14,000 instructions in front of a ~4,500 one. A
  call-site lane now answers it directly (guarded like the method-table lane, and checked against
  the full path in debug builds): a `pull-one` loop is 2.7x faster.
- **`my int $c = $a cmp $b`.** Storing an `Order` (or any `Int`-based enum value) into a native
  int took the type checker's role and MRO walk, ~7,000 instructions per assignment.
- **`++$i` on a `my uint $i`.** Only `int` slots stepped in place; a `uint` slot took the full
  by-name store. It now steps in place while the value stays non-negative.

ParaSeq: `t/08-grep` 113 s -> 71 s, `t/09-map` 57 s -> 30 s, `t/30-max` 16 s -> 11 s, `t/31-min`
19 s -> 12 s (rakudo 25 s, 6 s, 1 s, 1 s); the rest of #17337 is still open.
