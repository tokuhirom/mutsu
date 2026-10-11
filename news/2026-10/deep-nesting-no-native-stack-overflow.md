# Deeply nested runtime data no longer crashes the process

A structure built at run time — `$a = [$a]` a million times, a long chain of
Pairs or Hashes — used to walk the thread off its native stack when it died
(SIGSEGV in recursive `Drop`) or was printed (`.Str` / `.gist` / `.raku`
aborted with "stack overflow"). Both happened inside a single statement, out
of reach of the ADR-0100 call-boundary guard (#11217).

- **Drop is bounded.** Releasing a container word now counts the releases
  nested on the thread; past 256 it parks the word on a thread-local work
  list that the outermost release drains. The native depth no longer grows
  with the data.
- **The renderers stop inside the reserve.** The per-thread stack floor moved
  to the leaf module `native_stack`, so `.Str`, `.gist` and `.raku` can probe
  it. When the floor is reached they stop descending and record the overflow.
  The method-row dispatcher, `say` / `put` / `print`, the concat and
  `StrCoerce` ops and the call boundary then raise the catchable
  "Too deep recursion (out of stack space)" error. The truncated text is
  never used.
- **Cycle collection is iterative.** The Bacon-Rajan traversals (`mark_gray`,
  `scan`, `scan_black`, `collect_white`) use work lists instead of recursing
  once per edge.

`$a = 1 => $a` is a cycle in Raku (the Pair keeps `$a`'s container). Printing
it now raises the catchable error instead of aborting. Rendering it the way
rakudo does, as `(\Pair_N = ...)`, is #12707.
