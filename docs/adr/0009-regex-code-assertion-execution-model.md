# ADR-0009: Regex code assertions — run inline in the real interpreter, and keep LTM declarative

- **Status**: Accepted (2026-07-17). Parts A, A2 and B all implemented.
- **Context**: `roast/integration/advent2013-day18.t` test 10 (the last failure in that
  file) and PLAN §4 ⑤. Supersedes the root-cause analysis recorded in
  `TODO_roast/BLOCKERS.md` for that file, which attributed the symptom to backtracking
  (measurement below shows backtracking is not involved).

## Problem

A `<?{ … }>` code assertion in a grammar must, per Raku semantics, run **once per
cursor attempt, inline, in the real interpreter**, with real side effects that are
immediately visible to later assertions in the same match — and that persist even when
the overall parse ultimately fails.

mutsu does none of those three things. `advent2013-day18.t` needs all of them:

```raku
our @dups = ();
token card {<face><suit>
    <?{
        my $card = $/.lc;
        @dups.push($card) if %*PLAYED{$card};   # (a) outer array, (b) must survive parse failure
        ! %*PLAYED{$card}++;                    # (c) must be visible to the NEXT card's assertion
     }>
}
```

Test 10 asserts `@dups eqv ["a♥", "j♥"]` after three parses, **two of which fail** — so
the pushes from failing parses must persist.

## Measured characterization (2026-07-17, debug build)

Minimal grammar, minimal input — no backtracking possible:

```raku
our %n = ();
grammar C { token TOP { (\w) <?{ %n<C>++; True }> } }
C.parse("a");     # mutsu: assertion runs 4 times.  raku: 1
"a" ~~ /(\w) <?{ %n<E>++; True }>/;   # mutsu: 2.    raku: 1
```

The four runs, from a captured backtrace:

| # | Entry path | Purpose |
|---|---|---|
| 1 | `eval_token_call_values` → `regex_match_len_at_start` | LTM: candidate match length (runs the **whole** pattern) |
| 2 | `eval_token_call_values` → `declarative_prefix_match_len` → `regex_match_len_at_start` | LTM: prefix length (runs the whole pattern again) |
| 3 | `dispatch_package_parse` → `regex_match_with_captures_full_from_start` | the real match |
| 4 | `execute_regex_code_blocks` | replay in the parent interpreter on the winning path |

Runs 1–3 execute the assertion in a **scratch interpreter** (`eval_regex_code_assertion`
builds `Interpreter { env: self.env.clone(), .. Self::new_regex_scratch() }` and drops
it), because the matcher below `regex_match_with_captures_core` is `&self`. Run 4 is the
only one in the real interpreter.

**These four are not backtracking.** The earlier analysis (BLOCKERS.md) measured
`%seen{k}++` reaching 12 where raku reaches 2 and attributed the whole symptom to
backtracking. There are in fact *two independent* multipliers, and this ADR is about the
first:

1. a **constant 4**, present on a match that cannot backtrack at all (the table above), and
2. the **failure-position probe**, which is what produces day18's larger numbers (`a♥`
   reaching 15 on `"a♥ a♥ 7♦ 8♣ j♥"`, 31 on the two-hand input) — and which is *also* not
   backtracking. `parse_failure_for_pattern` → `longest_complete_prefix_end` re-matches
   the pattern against **every prefix** of the input, longest first, to report how far a
   failed `.parse` got. Each re-match executed the grammar's assertions again. The tell is
   exact: 14-char input → 15 runs, 31-char input → 31 runs. (15 = 2⁴−1 was a coincidence.)
   A backtrace over `token TOP { <item>**3 }` splits the 13 runs cleanly: **3 from the
   real match — exactly raku's count — and 10 from the probe.**

**mutsu's ratchet is not at fault.** Nothing here backtracks; the real match already runs
each assertion once per position, matching raku, on both successful and failing parses.

The constant is not depth-dependent — nesting `token TOP { <a> }` … four deep leaves it at
4, not 4ⁿ.

### Consequences observed today

- A **hash** element write from a scratch run leaks into the parent (it mutates the
  shared `Gc<HashData>` node in place). That is why `%*PLAYED` "works" — by accident —
  and why it is *over-counted* 4×.
- A **scalar** or **array** write does not leak (the scratch env is dropped). `@dups.push`
  resolves to a fresh local in the scratch block and is discarded. Hence `@dups` stays
  empty.
- On a **failing** parse there is no winning path, so run 4 never happens and no side
  effect survives at all. `enable_eager_code_blocks` exists for exactly this case, but
  its gate — `has_code_block_in_prefix(&pattern)` — inspects only the pattern's own
  top-level tokens and only counts plain `{…}` blocks (`is_assertion: false`). For
  day18's `^<TOP>$` (one subrule token, assertion nested four levels down) it is `false`.

## Decision

Two changes, in this order. The order is load-bearing: doing B first would *triple* every
side effect, because LTM would then execute assertions for real.

### A. LTM and trial matching must never execute a code atom

Rakudo builds its LTM NFA from the **declarative prefix** and never *executes* anything
while doing so. mutsu instead runs the real matcher over the whole pattern, twice, to
measure lengths (runs 1 and 2 above).

`roast/S05-grammar/protoregex.t` pins exactly how each code atom behaves in LTM, and the
two kinds differ:

```raku
token ass1:sym<a> { a <?{ 1 }> .+ }    token ass1:sym<b> { aa }
is ~LTM.subparse('aaa', :rule('ass1')), 'aaa', '<?{...}> does not terminate LTM';

token block:sym<a> { a {} .+ }         token block:sym<b> { aa }
is ~LTM.subparse('aaa', :rule('block')), 'aa', 'However, code blocks do terminate LTM';
```

So an **assertion does not terminate the prefix** — the NFA treats `<?{ }>` / `<!{ }>` as
a zero-width *pass* and keeps measuring past it (`a <?{1}> .+` has prefix `a .+`, which
beats `aa` on 'aaa'). Only a **plain `{ }` block terminates** it (`a {} .+` has prefix
`a`, so `aa` wins). Neither is ever run.

(An earlier draft of this ADR asserted that assertions terminate the prefix. That was
wrong, and `protoregex.t` — a whitelisted test — caught it immediately. mutsu's existing
`has_code_block_in_prefix`, which counts only `is_assertion: false`, had the distinction
right all along; its real defects are that it does not descend into subrules, and that
the measurement executes assertions rather than skipping them.)

The measurement must therefore:

- skip `<?{ }>` / `<!{ }>` as zero-width passes without executing them, continuing to
  measure the atoms after,
- stop at a plain `{ }` block without executing it, and
- do both through subrule references, not just within the pattern's own token list.

`regex_match_len_at_start` at `dispatch.rs:123` is used both as a *filter* (drop
candidates that do not match) and a *tie-break*. Rakudo does neither with a full trial
match: it orders by LTM prefix and lets the caller run the real match once, falling back
to the next candidate on failure. Reproducing that is part of this change.

### A — as implemented (2026-07-17)

Rather than truncating the token list (which cannot see a code atom nested inside a
subrule), the ordinary matcher runs under a thread-local `LTM_DECLARATIVE_MODE`. In that
mode the `CodeAssertion` arm of `regex_match_atom_with_capture_in_pkg` never executes the
code: an assertion returns a zero-width success and measuring continues, while a plain
block additionally sets `LTM_PREFIX_TERMINATED`, which `walk_tokens` checks on entry and
unwinds the DFS on. Because the flags are thread-locals they survive the nested
sub-interpreter dispatch that a subrule match goes through, so the prefix descends into
subrules and handles a code atom *anywhere* below — which is what the token-truncation
approach could not do. The old `has_code_block_in_prefix`-gated truncation is deleted.

One correction fell out of it: a declarative prefix can be used for **ordering only, never
for filtering**. `declarative_prefix_match_len` therefore returns `(len,
stopped_at_code_atom)`. When it stopped at a code atom, a `None` length proves nothing —
`token TOP { <+d>+ }` over `token d { <?{ True }> <[0..9]> }` measures as `None` because
`d` stops zero-width so `+` matches nothing, yet the real match succeeds — and the
candidate must be kept for the real match to judge. Only a *fully declarative* candidate
that fails to match may be dropped. (Pinned by `t/regex-classitem-token-fallback.t`, which
caught exactly this when the filter was first swapped over.)

Measured after A: the constant drops **4 → 2** (`.parse`) and **2 → 2** (plain `~~`),
uniformly and independent of nesting depth. The remaining 2 are the real match plus the
parent replay — that is B's job. raku is 1.

### A2. The failure-position probe must not execute a code atom either

Same principle, different caller. `longest_complete_prefix_end` re-matches the pattern
against every prefix of the input purely to report how far a failed `.parse` got. That is
a diagnostic; building it must not run the user's grammar code once per prefix.

It runs under a `CODE_ATOMS_INERT` flag, so both kinds of code atom become zero-width
no-ops and the probe measures the pattern's declarative skeleton. It deliberately differs
from `LTM_DECLARATIVE_MODE`: a plain `{ }` block does *not* stop the walk here, because
the probe wants the longest prefix the skeleton accepts, not the LTM prefix.

Note the ordering interaction: **A made this worse before it was fixed.** Before A, an
assertion-bearing candidate that failed its trial match was dropped by the LTM filter and
`.parse` returned early with `pattern: None`, never reaching the probe. A keeps such
candidates (correctly — a declarative measurement cannot filter), which exposed the probe
on exactly the grammars that have assertions. Measured on `token TOP { <group>**2 % ';' }`
over an 18-char failing input: `a` ran **19** times, `z` **31**; after A2, 1 each — raku's
numbers exactly.

### B. Code assertions run inline in the real interpreter

Flip the matcher from `&self` to `&mut self` from `regex_match_with_captures_core` down
(~65 methods across `src/runtime/regex/`, 9133 lines), and have
`eval_regex_code_assertion` evaluate in `self` rather than a scratch clone. The boundary
is already `&mut self` one frame up (`regex_match_with_captures`), and `parse_regex`
returns an `Arc<RegexPattern>` — an owned handle, not a borrow of `self` — so the
recursion does not hold a `self` borrow across the recursive call.

This deletes the scratch-plus-replay dual mechanism for assertions:
`execute_regex_code_blocks` no longer replays them, `enable_eager_code_blocks` is no
longer needed to make failing parses have side effects, and the hash-leak accident stops
being load-bearing.

### B — as implemented (2026-07-17)

The mechanical flip cost far less than feared: 47 `&self` methods, and the borrow checker
barely fought back, because nothing in the matcher returns a reference borrowed from
`self` (`WalkCtx` borrows the `Arc<RegexPattern>` and the `current_package()` String, both
locals). Only two closures needed work — both captured `self` mutably and so locked out
interleaved `self` calls: `walk_quant_chain`'s `grow_one` became the `grow_one_iter`
method, and `match_class_item` became an explicit `FnMut`. A handful of `&self` methods
that only read the registry (`resolve_token_patterns_static_in_pkg`,
`collect_token_patterns_for_scope*`) were reverted from the blanket flip.

`eval_regex_code_assertion` now evaluates in `self`. Only the regex-internal bindings it
installs (`$/`, `$0`…, `$<name>`) and the assertion's own top-level `my` declarations are
saved and restored around the body — `eval_block_value` does not scope plain lexicals
(mutsu's env is flat), so without the latter day18's `my $card` would clobber a same-named
outer variable. Every other write is left in place: that is the point.

Assertion runs are now **1 per matched position — exactly raku's count** — for every shape
measured (inline, one subrule down, four subrules down, plain `~~`), completing the
4 → 2 → 1 progression from A and A2.

## Alternatives rejected

- **Widen `has_code_block_in_prefix` to see nested assertions and enable the eager buffer
  for day18.** This is the smallest diff that could turn test 10 green, and it is a
  band-aid: replay-after-the-fact cannot give (c) — an assertion's side effect being
  visible to a *later assertion in the same match* — which day18 depends on. It would
  pass the test while leaving the model wrong, and it keeps a dual mechanism that
  ADR-0001's "gain" definition tells us to remove.
- **Deep-isolate the scratch interpreter** so trial runs have no side effects at all.
  Removes the hash-leak accident but also removes the only reason `%*PLAYED` works today,
  and still cannot make real side effects happen. Strictly worse.
- **Keep `&self` and use interior mutability (`RefCell<Env>`) for the assertion path.**
  Avoids the mechanical refactor but hides re-entrancy behind a runtime borrow that can
  panic mid-parse, and leaves the "assertions are special" split in place.

## Consequences

- **Cost**: B is a large mechanical refactor with a wide blast radius (the whole regex
  and grammar surface). CI + roast is the safety net; expect several red iterations.
- **Perf**: A is also a perf fix, and a bigger one than the assertion angle suggests —
  it removes one of *two* full trial matches per subrule resolution for **every** grammar,
  assertion-bearing or not. ADR-0007 recorded a "still-unexplained runtime regex re-parse
  path (`parse_regex_uncached` + LTM expansion ~4% in-profile)"; this is very likely a
  large part of it. Measured (release, interleaved A/B, median of 9, 2000 parses of a
  64-char JSON doc with a 6-candidate proto): **2.15 s → 1.61 s = ×1.34**. Not from the
  bench CI, so it is a development-decision number, not a documentable one — the
  `bench-grammar-parse` benchmarks finish in ~0.02 s and are far too small to resolve it.
- **Risk**: assertions gaining real side effects will surface latent double-execution
  elsewhere. That is the point — those are deterministic roast failures, not flakiness.
- **Scope**: this does not attempt Rakudo's full NFA-based LTM. A is only "stop running
  assertions during length measurement"; genuine NFA prefix construction stays out.

## Status of the work

- **A: done** (2026-07-17, #4651). Pinned by `t/regex-ltm-declarative-prefix.t`.
- **A2: done** (2026-07-17). Pinned by `t/grammar-parse-failure-probe.t`. With A+A2,
  `advent2013-day18`'s assertion counts match raku **exactly** (2 and 1 on the two failing
  parses, 1 per card on the passing one).
- **B: done** (2026-07-17). Pinned by `t/grammar-assertion-side-effects.t`.
  **`roast/integration/advent2013-day18.t` now passes 10/10**, test 10 included.

All three pins pass under raku as well as mutsu, and fail on the tree before their fix.

## Known gap this does NOT close

A **file-scope** variable is invisible to a code assertion: it lives in the top VM frame's
locals and never reaches the env the regex engine reads, so the assertion sees `Nil`.

```raku
our @a = ('x','y','z');                                   # file scope
grammar G { token TOP { (\w) <?{ … @a … }> } }            # assertion sees Nil
{ our @b = ('x','y','z'); my grammar H { … <?{ … @b … }> } }   # block scope: sees @b
```

This is the `env_dirty` dual-store debt, is **unchanged by B** (measured identical before
and after — the scratch cloned the same env, which also lacked the name), and is why
day18 works: its `our @dups` is declared in a block alongside the grammar. Only `~~`
matching syncs locals into env (`sync_regex_interpolation_env_from_locals`), and even that
skips names not already in env. Closing it belongs with the dual-store work, not here.
