# `~~` no longer publishes the whole frame for EVAL-in-regex or a Proxy/lazy RHS

#9169 cut the `~~` op's per-match env publish down to the locals a regex,
a substitution and their embedded code can name. Two shapes still fell back to
publishing every local slot of the frame, O(L) per match (#9293):

1. Code embedded in a regex or an `s///` replacement that looks a name up
   indirectly: `EVAL`, `::($n)`, `callframe`, the pseudo-packages (`MY::`,
   `OUTER::`, ...). No scan of the text can bound what such code reads.
2. An RHS value whose elements need user code to reach: a lazy list, a
   `Proxy`, a lazy thunk.

## Case 1: the same mechanism as a plain `EVAL`

A plain `EVAL '$x'` outside a regex has always read a caller lexical through
the store-side env mirror. A chunk that calls `EVAL` (or does any other
reflective lookup) is marked reflective when it is finalized
(`CompiledCode::scan_reflective_name_access`). That sets the process-wide
latch that keeps every local store mirroring into `env`, and the chunk's
`needs_reflective_capture`.

The same lookup inside a regex was invisible to that scan: it is compiled from
the regex text at match time, not into the chunk's ops. So `~~` compensated
by publishing the whole frame before every match. Now the finalize scan also
reads the chunk's regex constants and its substitutions' patterns and
replacements (`holds_indirect_regex_lookup`, via
`vm_smartmatch_sync::regex_source_has_indirect_lookup`). A chunk holding such
code becomes reflective exactly as if it called `EVAL` directly, and its
locals stay env-synced. `~~` then publishes only the names spelled in the
text, as for any other code-bearing regex.

## Case 2: read the RHS the way the match does

- A lazy list RHS answers `False` without being reified (as in Rakudo), so the
  match runs none of its regexes.
- A nested `Proxy` or an unforced thunk is compared as it stands, so nothing
  in it runs either.
- A top-level `Proxy` RHS is now FETCHed before the match, and a top-level
  lazy thunk is forced. This mirrors what the op already did for the LHS.
  It also fixes a wrong answer: `"ab" ~~ $p` with
  `$p := Proxy.new(FETCH => -> $ { /a$w/ }, ...)` answered `False` instead of
  Rakudo's `True`.

The fetched or forced value, or a forced thunk's cached value, is then
searched for regexes like any other value. The depth guard that used to give
up with a whole-frame publish now switches to a visited set past depth 64. A
self-referential RHS still terminates, and no fallback is left.

## Result

`scripts/vm-complexity-check.sh` gains four cases: EVAL in `<?{ }>`,
`::($n)` in `<?{ }>`, a Proxy RHS and a lazy list RHS. With the frame's
locals doubled, the `::($n)` and lazy-list cases went from 1.98x and 2.24x
to flat (1.05x, 1.04x). The EVAL case's own ~3 ms per match compile cost
hides its growth. The Proxy case went from 2.10x to 1.59x. What remains is
the FETCH itself, not `~~`: a plain `$v = $p` read grows the same way, and
that is filed as #9385. The `Rakudo: O(1) + the RHS`
suffix is gone from `SmartMatchExpr`.

Pin: `t/regex/match/smartmatch-indirect-lookup-sync.t`. It checks EVAL /
`::()` reads after `++`, `+=`, `push`, hash stores, loop parameters,
inner-block and routine locals, closures, `s///` replacements, a computed
regex, and the Proxy and lazy RHS cases.
