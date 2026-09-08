# The `<subrule>` calls the streamed path declines are the last collect-then-pick barrier

**Narrowed 2026-09-08** by ADR-0073's streamed half
(`news/2026-09/subrule-calls-stream-their-candidates.md`), which closed the
headline rows this file was opened for: E1, E2 and E6 all match `raku` now, and
non-leaf rules under a ratcheted caller no longer compute an end set their caller
cannot use. Earlier narrowings: Slice 1+3
(`news/2026-09/regex-atom-candidates-are-demand-driven.md`) and Slice 2's
ratcheted half (`news/2026-09/ratcheted-subrule-calls-are-first-only.md`).

## What is left

`drive_named_subrule_candidates` (`src/runtime/regex/regex_match_lazy.rs`) takes
one shape and one only: an argument-less call resolving to exactly one non-proto
candidate, with no custom-HOW dispatch, no `:m`, no dynamic (`$*`) rule
parameters declared anywhere in the program, a key that is not already LR-active,
and a rule the call graph (`regex_call_graph.rs`) proves cannot reach a call to
its own name. Everything else still goes to the eager `Named` arm in
`regex_match_atom.rs`, which computes a subrule's whole end set before the caller
descends into any of it — so an embedded `{ … }` block inside such a subrule
still runs once per end *computed* rather than once per end *entered*.

The declined shapes, roughly in descending order of how often they come up:

1. **A rule that really is part of a call cycle.** Left recursion is the reason
   the growing-seed loop exists, and the loop needs the whole candidate set to
   decide whether the seed grew. Streaming here means threading the continuation
   *through* the seed loop, so that a candidate is produced, offered, and only
   then followed by the next iteration's — with no way to un-run what the
   rejected ones already did. This is the genuinely hard residue.
2. **A proto/`multi` subrule.** ADR-0046's rank-then-match dispatch is already
   demand-driven at the *candidate* level, but it wants the winning candidate's
   whole end set. Streaming it means driving the ranked candidates and their
   ends from one continuation.
3. **Several resolved candidates without a proto.** The eager arm deduplicates
   ends *across* candidates (first occurrence per end wins); a streamed form has
   to interleave the candidates' walks to preserve that priority order.
4. **A call with arguments** (`<expr($p-1)>`). The arguments are part of the
   left-recursion key and are evaluated per call, so the memoized call-graph
   verdict does not apply; the analysis answers "may re-enter" for any
   argument-bearing edge, which also makes every *caller* of such a rule
   ineligible.
5. **A body that splices a value into its own pattern text.** A `Regex`-valued
   scalar is interpolated as pattern SOURCE, so the rule's call edges are not a
   property of the token generation and the analysis refuses to cache — it
   answers "not knowable" instead. (A sigil inside a `{ … }` code block does not
   count; parse-time interpolation treats code blocks as opaque.) Tightening this
   means tracking *which* interpolations can introduce a rule call.
6. `<::(EXPR)>` symbolic indirection, `<{ … }>` closure interpolation, `<~~>`,
   a name that resolves to a grammar **method**, a custom-HOW grammar, `:m`, and
   a program that declares any dynamic (`$*`) rule parameter — each of which
   returns from a different place in the arm, or changes the dynamic scope the
   continuation would run under.

## Why the residue is not obviously worth clearing

Every item above is *correct* today; what it costs is extra `{ … }` block runs
on paths raku never enters, which only bites a block with a side effect that must
not happen — a `die`, a push, a counter — plus the wasted enumeration. Before
opening any of these, measure how often the declined shapes actually occur: the
streamable verdict is memoized per (package, subrule atom text) in
`regex_call_graph.rs`'s `STREAMABLE`, so a counter there would say directly which
of the six is worth the work.

Note also that the analysis is deliberately a *sound under-approximation*: it
answers "may re-enter" for anything it cannot resolve. Widening it is the cheap
half of items 4-6; the streaming machinery is the expensive half of items 1-3.

## Also still eager (measured, different mechanisms)

- The `:g` / `subst` scan and the counted-separator chain bug have their own
  ticket files (see the Slice 1 news entry's Residue section).
