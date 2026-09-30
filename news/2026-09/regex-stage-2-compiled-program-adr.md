# Regex Stage 2 decided: compile to a flat backtracking program

ADR-0099 left its Stage 2 as a question: re-profile grammars once the ceremony around the regex
engine is gone, and ask whether the tree walk has become the majority cost. [ADR-0135](../../docs/adr/0135-regex-compiles-to-a-backtracking-program.md)
answers it ([#9915](https://github.com/tokuhirom/mutsu/issues/9915)).

The re-profile found that every loss ADR-0099 measured against warm rakudo is gone. The
suite grammar parses at 2.91 µs/char against rakudo's 7.10 (it was 14.4 against 7.7), and a small
`~~` is at parity. The walk, though, is now ~88% of a grammar parse, against under 12% then, and
its self-cost is spread thin across allocation, thread-local side channels and five
candidate-generator layers.

To size the gain, a ~150-line flat backtracking VM was built as a throwaway prototype and run on
the same 640 KB subject. On failing scans that no prefilter can help, it took 11.7 ms and 33.4 ms
where mutsu and rakudo both take 370 ms and 1,350 ms, a 32-40x gap. The ADR proposes:

- an `RxProgram` per pattern, memoized in `PatternDerived`;
- one backtracking loop with an explicit backtrack stack over the existing `CapStore` trail;
- subrule calls as frames in the same loop, which makes every call demand-driven and folds in #7548;
- a differential mode that runs both engines while they coexist;
- deleting the walk as the completion criterion.

It lands in five slices, the first with a kill criterion.
