# ADR-0111: An LTM stopper ends one path of the measurement, not the whole walk

- **Status**: Accepted (2026-09-23); implemented in the same PR as the decision.
- **Amends**: [ADR-0022](0022-regex-alternation-ltm-ranking.md) §4.2, the meaning of
  `LtmAtomMode::Terminate`. The rest of ADR-0022 (what counts as a stopper, the
  `litlen` tie-break, the "a terminated measurement can order but never filter"
  contract) is unchanged.
- **Context**: [#9053](https://github.com/tokuhirom/mutsu/issues/9053) —
  `CSS::Module::CSS3::Selectors` `negation-expr` took the catch-all `<any-arg>` branch
  where Rakudo takes the earlier `<qname>`.

## 1. Problem

ADR-0022 §4.2 specified a stopper (`<.ws>`, a code block, a subtraction class, ...) as a
zero-width success that sets `LTM_PREFIX_TERMINATED`, after which `walk_tokens` accepts the
current position and unwinds the entire depth-first walk. The measured prefix was therefore
the position of **the first stopper the walk happened to reach**.

Rakudo builds an NFA from the prefix and advances every path together. A stopper puts a
*fate* on its own path only, and the prefix is the **furthest** position at which any path
reaches a fate or the end of the pattern. The two agree when there is one path. They
disagree whenever a stopper sits on one branch of an alternation or inside a loop, and a
different path goes further:

```raku
token nmchar { <[a..z]> | <-[Z \n]> }   # branch 2 is a fate (see §3)
token Id     { <[a..z]> <nmchar>* }
rule  qname  { <namespace-prefix>? <element-name> }   # namespace-prefix = [<Id>|...] '|'
```

On `"pq"`, Rakudo's paths through `qname` reach fates at offsets 1 and 2 (the second
branch of `nmchar` fires after every character), so the prefix is 2. mutsu stopped at the
first fate, offset 1, and ranked `qname` below a sibling whose prefix was 2.

## 2. Decision

A stopper **records its position as a fate and fails its own path**. The walk continues
through every other path. Measurement entry points open a *fate frame*, and the measured
prefix is `max(furthest full match, furthest fate)`. Mechanism: `regex/regex_ltm_fate.rs`.

- `ltm_record_fate(pos)` also sets `LTM_PREFIX_TERMINATED`, which now means "some path of
  this measurement reached a fate". It no longer stops anything, so the per-sibling resets
  that PR #9087 added to both `Alternation` arms (to stop the old flag leaking between
  branches) are gone, and `walk_tokens` has no entry check.
- `TerminateAfter(X)` (`<?before X>`) records a fate at every end of `X`, which is what
  the NFA's inlined `X` followed by a fate edge does. A path of `X` that fails records
  nothing (the old code terminated at `pos` in that case).
- Positions are indices into the character array being walked. The matchers that walk a
  transformed or re-sliced subject open their own frame and map its fate back when they
  close it: a no-capture subrule matched on `&chars[pos..]` (+`pos`), `:m` mark stripping
  (the stripped map), and `:i` multi-character case folding (the fold map).
- A measurement never scans: the string entry points (`declarative_prefix_match_len`,
  the no-capture subrule prober) only try start 0 while `LTM_DECLARATIVE_MODE` is set, since
  a later start would record fates for a start the candidate never has.
- The ratchet fast paths in `walk_ratchet_fast_paths` are skipped under measurement: the
  NFA ignores `:ratchet`, and those loops stop at the end of the subject without trying
  the atom, which hides a fate reachable there.

## 3. Also decided: a negated class with several alternatives is a fate

Rakudo's `cclass_elem` action folds the plain entries of one `[...]` into a single
`enumcharlist` and gives every range, class escape (`\d \w \s` and negations), negated
escape (`\N \H \X[..] \C[..]`), and `\n` (which also matches `\r\n`) an alternative of its
own. With two or more alternatives a *negated* class compiles to `[<?conj> .]`, and the NFA
has no method for `conj`. So `<-[Z \n]>`, `<-[a..c x]>`, `<-[\d x]>` are fates, while
`<-[\n]>`, `<-[a..c]>`, `<-[\t \r x]>` and `<-[\x0a x]>` stay declarative (all measured
against `raku`). mutsu parses such a class into the existing
`CompositeClass { positive: [], negative }` form, which matches the same characters and
which `ltm_atom_mode` already terminates on (`regex_parse_charclass_alts.rs`).

## 3a. Also decided: a package-qualified subrule call is a fate

`<CSS::Grammar::Core::_arg>` (and even `<G::x>` naming the grammar itself) reaches
`mergesubrule` as the name `CSS::Grammar::Core::_arg`. `tryfindmethod` on the cursor finds
no method by that name, so no sub-NFA is merged and the path gets a fate at the call
(verified against `raku`). mutsu resolved and measured through it. `ltm_atom_mode` now
terminates on a `Named` atom whose interned lookup name is qualified. This was the rest
of #9053: CSS::Grammar's `any-arg` is `rule {<CSS::Grammar::Core::_arg>}`, so its prefix is
0 in Rakudo and `qname` (prefix 1 via the fate in `nonascii`) wins outright.

## 4. Alternatives rejected

- **Share the subrule `%seen` guard across sibling branches.** The previous investigation
  on #9053 suspected Rakudo's `mergesubrule` cycle guard. NQP's `NFA.nqp` clones `%seen`
  per call, so it is per path; the source refutes it.
- **Mark terminated ends on the match results** so the caller stops only at those. It needs
  a flag carried through every capture delta and every subrule boundary, and it still has to
  decide what to do when one position is both a terminated end and a normal one. Recording
  fates as a number beside the walk is the NFA's own shape.

## 5. Known gaps

- Uniprop atoms (`<:L>`, `<-:L>`) and a negated named class (`<-alpha>`) are fates in
  Rakudo (no NFA method), and are still measured as declarative here.
- `<-[\s] + [x]>` fails to match at all in mutsu (a class-union bug, independent of LTM).
