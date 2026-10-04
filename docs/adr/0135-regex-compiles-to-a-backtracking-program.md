# ADR-0135: A regex compiles to a flat backtracking program; the tree walk is retired

- **Status**: Accepted (2026-09-30; proposed and accepted the same day); Slices A and B landed, Slices C and D in part, Slice E begun (§8). Slices tracked as
  [#10251](https://github.com/tokuhirom/mutsu/issues/10251) (A),
  [#10252](https://github.com/tokuhirom/mutsu/issues/10252) (B),
  [#10253](https://github.com/tokuhirom/mutsu/issues/10253) (C),
  [#10254](https://github.com/tokuhirom/mutsu/issues/10254) (D),
  [#10255](https://github.com/tokuhirom/mutsu/issues/10255) (E).
- **Supersedes in part**: [ADR-0099](0099-regex-engine-performance-strategy.md) §4 Stage 2
  ("deferred, not decided") and Stage 3 ("the CPS→bytecode regex VM stays deferred"). ADR-0099's
  Stages 0 and 1, its measurements and its rejected alternatives stand.
- **Decides**: [#9915](https://github.com/tokuhirom/mutsu/issues/9915). **Folds in**:
  [#7548](https://github.com/tokuhirom/mutsu/issues/7548) (the streamed-subrule barrier).
- **Relates to**: [ADR-0007](0007-grammar-parse-trail-matcher.md) (cursor + trail — kept),
  [ADR-0009](0009-regex-code-assertion-execution-model.md) (code assertions — kept),
  [ADR-0016](0016-span-based-captures-and-lazy-match.md) (span captures, lazy `Match` — kept),
  [ADR-0073](0073-regex-atom-candidates-are-demand-driven.md) (demand-driven candidates —
  replaced by the backtrack stack), [ADR-0022](0022-regex-alternation-ltm-ranking.md) /
  [ADR-0046](0046-proto-token-ltm-shares-one-ranking-mechanism.md) /
  [ADR-0127](0127-every-ltm-measurement-runs-the-nfa.md) (LTM — kept, consulted by one op),
  [ADR-0133](0133-no-per-call-ast-compile-at-runtime.md) (no per-call AST compile),
  [ADR-0004](0004-jit-strategy.md) (Cranelift JIT — the later consumer of the flat form).

## 1. Where ADR-0099 left the question

ADR-0099 declined to decide Stage 2 because the walk was under ~12% of a grammar parse and ~8% of a
small match: the measured losses were ceremony around the engine, not the engine. It asked for a
re-profile after Stage 0, with the question "has the walk become the majority cost once the
ceremony is gone?". Stage 0 and Stage 1 are both implemented (ADR-0099 §8), and #8450
(`news/2026-09/regex-per-position-reject-cost.md`) shaved the per-position reject cost and closed
by naming what is left: "the walk itself: `walk_quant_chain` → `grow_one_iter` →
`regex_match_atom_with_capture_in_pkg` → `for_each_atom_candidate` →
`regex_match_atom_in_pkg_inner`, five layers deep, each moving a `RegexCaptures` by value … That is
the compiled-form question".

## 2. Re-measurement (release, 2026-09-30, `origin/main` at 9d1d0160; rakudo 2026.07)

Every figure is warm: steady-state iterations of an in-process loop (median of the late ones), or
best of several runs, per ADR-0099 §2's methodology.

### 2.1 Every loss ADR-0099 measured is gone

| workload | mutsu | rakudo | ADR-0099 (mutsu vs rakudo) |
|---|---:|---:|---|
| `bench-grammar-parse-big` grammar, 10,453-char document, 30 in-process parses | **2.91 µs/char** | 7.10 µs/char | 14.4 vs 7.7 |
| `"a" ~~ /a/`, 200k-iteration loop | 2.48 µs | 2.24 µs | 4.9 vs 1.78 |
| one ~70-char log line, `/ 'user=' (\w+) .* 'took=' (\d+) 'ms' /` | 12.8 µs | 12.6 µs | — |

mutsu parses the suite grammar 2.4x faster than warm rakudo, and is at parity on small matches.

### 2.2 The walk is now the majority — ADR-0099's trigger, answered yes

Callgrind over `bench-grammar-parse-big.raku` (241.4 M instructions, of which 12.0 M are
`mutsu -e 'say 1'`): ~21,900 instructions per document character, and the regex engine is **~88%
inclusive** of the whole run (`walk_tokens`, `for_each_atom_candidate`, `subrule_candidate_ends`,
`drive_named_subrule_candidates` all sit at 86–88%). ADR-0099 measured the same share at under ~12%.
The self-cost is not concentrated anywhere — the signature of interpretive overhead spread across
layers rather than one fixable hotspot:

| self cost | share |
|---|---:|
| `malloc` + `_int_malloc` + `free` + `_int_free` + `memcpy` | 17.9% |
| `LocalKey::with` (thread-local side channels) | 5.8% |
| `LtmNfa::run` | 4.5% |
| `regex_match_atom_all_with_capture_opts` | 4.2% |
| `walk_tokens` | 3.6% |
| `for_each_atom_candidate` | 2.9% |
| `match_consuming_atom`, `regex_match_atom_with_capture_in_pkg_inner`, `CapStore::merge_delta`, `regex_walk_ends_in_pkg`, `build_named_candidates_from_inner`, … | 1–2.4% each |

### 2.3 How much faster a compiled form is: measured with a prototype, not guessed

To bound the gain before deciding, a ~150-line flat backtracking VM was written in Rust: an
instruction vector (`Char`, `Word`, `Digit`, `Space`, `Split(a, b)`, `Jmp`, `Save(n)`, `Match`),
one `loop { match prog[pc] }`, and one explicit backtrack stack of `Resume(pc, pos)` /
`Restore(slot, old)` entries reused across start positions. It ran the same patterns over the
same 640 KB subject (the 46-char unit of ADR-0099 §2.4, repeated) as mutsu and rakudo. Patterns
were chosen so that no prefilter can help (no required literal); these are the shapes where the
walk itself is the cost.

| 640 KB subject, best of 3 (ms) | mutsu | rakudo | prototype |
|---|---:|---:|---:|
| failing `/ \w+ \s \d ** 6 /` | 370 | 385 | **11.7** |
| failing `/ [ \w+ \s ] ** 3 \d ** 6 /` | 1,350 | 1,320 | **33.4** |
| `.match(/ (\w+) \s (\d+) /, :g)`, 27,886 matches | 470 | 2,400 | 13.4 |
| `.comb(/ \w+ /)`, 139,430 matches | 55 | 330 | 2.7 |

The first two rows are like for like (a failing boolean scan: no result objects), and show a
**~32–40x** gap. The last two rows are not: mutsu and rakudo build `Match`/`Str` values for every
hit and the prototype only records spans, so they bound the matcher's share rather than predict
the end-to-end figure. For scale, regex-automata's `PikeVM` (a Thompson NFA simulation, in the
dependency tree through `regex`) takes 23.5 and 47.0 ms on the first two rows — a flat
backtracker over the same program is the faster of the two on these shapes.

Per unanchored start position, mutsu spends **~6,830 instructions** on `/ \w+ \s \d ** 6 /`
(callgrind, 40 KB subject, net of building the subject); the prototype spends ~18 ns. The
prototype does not implement graphemes, `:i`, LTM or captures-as-`Match`, so the end state will
not reach 32x; but ADR-0099's conclusion that the walk had nothing worth taking is overturned. The
remaining regex cost *is* the walk, and a flat program is an order of magnitude cheaper to run.

### 2.4 Grammar headroom is estimated, not measured

A grammar parse pays per-subrule costs a flat program does not remove by itself: candidate
resolution, `CapNode`/`Match` construction, LTM ranking of proto candidates, action dispatch. The
§2.2 profile says those are a minority, and the allocation share (17.9%) comes largely from the
by-value `RegexCaptures` deltas and candidate `Vec`s that §3 D2/D3 eliminate. Slice D (§4) measures
grammar headroom before it lands; no grammar figure is claimed here.

## 3. Decision

**Compile every regex to a flat instruction program run by one backtracking loop, and retire the
tree walk.** Raku regex stays a backtracking language with interpreter callbacks — this is not a
DFA, and nothing regular-language-only is introduced.

### D1. The program

A `RegexPattern` compiles to an `RxProgram`: a `Vec<RxOp>` plus side tables (character classes,
literal strings, capture names, LTM tables, code-block handles). It is memoized in the pattern's
`PatternDerived` (one `OnceLock`, like the Stage 1 prefilter), so every holder of a cached pattern
shares one program. A program is a pure function of its pattern: a `<subrule>` compiles to a call
op carrying the `NamedAtom`, and the call is resolved at run time (below), so no program is keyed
by package.

`RxOp` is its own instruction set, separate from the main VM's `OpCode` (§5 says why). Every op's
cost is annotated per `docs/complexity-annotations.md`.

### D2. The machine

One `RxVm` loop per engine entry, with registers `pc`, `pos`, the subject (`MatchTarget`, unchanged),
the existing `CapStore` and its undo trail (ADR-0007, unchanged), and one explicit **backtrack
stack** of `(pc, pos, trail mark, frame depth)` entries. Choice points (`|`/`||` branches, quantifier
iterations, frugal steps) push an entry; failure pops one, rewinds the trail to its mark and
resumes. Ratchet (`:r`, `token`, `rule`) is a *cut*: dropping the backtrack entries pushed since a
recorded height.

Captures are written into the `CapStore` directly. No `RegexCaptures` value is built per atom, per
candidate or per start position; the per-subrule `Match` is built once, from spans, when the
subrule returns (ADR-0016's lazy `Match` is unchanged). This removes the delta-and-merge traffic
that is most of §2.2's allocator share.

### D3. Subrule calls are frames, not Rust recursion

A `<subrule>` call resolves the callee through an inline cache keyed on (invocant package,
`TOKEN_DEFS_GEN`) — the same key the Stage 1 prefilter uses — and pushes an `RxFrame` (return pc,
callee program, frame's capture store, entry pos) onto the VM's own call stack. The callee runs in
the same loop. Its choice points go on the **same** backtrack stack, delimited by the frame:

- a ratchet callee (`token`/`rule`, the grammar common case) cuts them when it returns;
- a non-ratchet callee (`regex`) leaves them, so a later failure in the caller resumes *inside* the
  callee — Rakudo's bstack model.

This makes every subrule call demand-driven by construction. The eager `Named` arm and
`drive_named_subrule_candidates`' eligibility analysis (#7548: several candidates, arguments,
interpolating callees, dynamic parameters, `:m`) stop existing, rather than being widened case by
case. The exception is a call that is **left-recursive**: mutsu supports left recursion (rakudo does
not), and seed-growing needs the callee's whole end set per iteration. That runs through one op,
`LrCall`, which drives the callee to exhaustion in a nested run. It is the only eager construct
left, and it is confined to a call the call graph (`regex_call_graph.rs`) proves re-enters itself.

No Rust recursion per regex nesting level means the stack depth of a match no longer grows with
subject length or grammar depth.

### D4. One definition of what an atom matches

The single-implementation rule ([ADR-0117](0117-str-methods-and-nqp-ops-share-one-routine.md)) holds
inside the regex engine. An op tests the subject through the functions the walk calls today:
class membership (`CharClass`, `ClassItem`, the cclass table), `grapheme_end`, case folding
(`regex_casefold`), Unicode properties and the `:m` stripped view. The compiler chooses *which* test
to run; it never restates *what* a test accepts. In particular:

- **LTM.** `|` alternation and proto dispatch compile to one `LtmAlt` op that asks the existing
  `LtmNfa` (ADR-0127) for the ranked branch order and pushes the branches in that order. ADR-0022,
  ADR-0046 and ADR-0127 decide what the ranking is; this ADR only changes who consumes it.
- **Code.** `{ … }` blocks, `<?{ … }>`, `<{ … }>`, `:my` and interpolation run the precompiled
  closure on the caller's interpreter exactly as today (ADR-0009, ADR-0133). The op is a call-out;
  the run count per path is the demand-driven one D3 guarantees.
- **Graphemes.** An op consumes a grapheme wherever the walk does, including the ASCII fast path
  #8450 added.

### D5. Migration: per pattern, with a boundary at subrule calls

A pattern compiles if every atom in it is supported by the slices landed so far. Otherwise it keeps
the walk. There is no per-atom escape op back into the walk in the middle of a program; mixing two
execution models inside one pattern's backtracking would be the hardest code in either engine. The
two engines meet only at a subrule call: a compiled caller can call a walked callee through a bridge
op (the callee's ends are enumerated with ADR-0073's `MatchSink::Cont`), and a walked caller reaches
a compiled callee through the same entry point every other caller uses.

A `MUTSU_VM_STATS` counter reports compiled vs. walked patterns, with the reason a pattern declined,
in the shape of `scripts/subrule-stream-survey.sh` (#7548). That count is the migration ratchet.

### D6. Soundness: a differential mode, gated in CI

Two engines over one language drift silently, and this is the risk ADR-0099 §7 warned about for
Stage 1 ("wrong without being incorrect"). So while both exist, `MUTSU_RX_DIFF=1` runs every
compiled match through the walk as well and aborts on any disagreement: match or no match, end
position, every capture span, and the sequence of code-block invocations. Each slice passes
`t/regex/`, `t/grammar/` and the whitelisted `roast/S05-*` files under it before landing, and CI
runs that subset in differential mode for as long as the walk exists. The mode is deleted with the
walk.

### D7. The end state is one engine

The walk is deleted when the D5 counter reads zero walked patterns over the roast whitelist and
`t/`. That deletion is this ADR's completion criterion, not an optional clean-up. Two engines are
transitional debt with a defined exit; they are never the design.

*Sharpened 2026-10-01 (#10255).* The pattern counter alone cannot say when the walk is deletable:
a compiled pattern still reaches the walk's code at run time when the dynamic context keeps the
engine out, when a `<subrule>` call bridges, when an atom is matched by the walk's single-atom arm,
and through every entry point that has no compiled form (all ends at a position, behind `:ov`/`:ex`,
LTM lookahead fates and cursor token methods). The criterion is therefore read off the second
counter line, `regex-walk:` (§8, "Every use of the walk, counted"): the walk is deleted when its
`walked=` and `bridged=` totals read zero over the roast whitelist and `t/`, and every remaining
`leaf=` reason names a primitive that has moved out of the walk's modules.

### D8. JIT is later, and separate

The flat program is the precondition for lowering hot regexes through Cranelift (ADR-0004). That
lowering is **not** decided here; it needs its own ADR, triggered by a measured case where the
`RxVm` dispatch loop itself is the majority cost.

## 4. Slices

Each slice lands on its own, passes `scripts/dev gate` and the D6 differential subset, and records
its measured result in §8 of this ADR.

- **A. Machine, compiler, regular core.** `RxOp`/`RxVm`/`RxProgram`, the `PatternDerived` slot, the
  D5 counter and the D6 mode. Atoms: literals (char and grapheme), classes, Unicode properties,
  `.`, anchors and boundaries, groups, positional and named captures, greedy/frugal/ratcheted and
  counted quantifiers, `%`/`%%` separators, `||`, backreferences. No subrule calls and no code.
  Entry points: `~~`, the unanchored scan behind the Stage 1 prefilter, `:g`, `.comb`, `.subst`,
  `.split`. **Kill criterion**: the §2.3 scan rows must improve at least 5x end to end. Below 3x,
  stop and revise this ADR before Slice B.
- **B. The rest of the declarative language.** `|` via `LtmAlt`, `:i`, `:m`, lookaround,
  conjunction.
- **C. Code.** Code blocks and assertions, `:my`, closure and variable interpolation.
- **D. Subrule calls.** D3's frames and inline cache, ratchet callees first; proto dispatch; the
  `Match` tree and action dispatch. The grammar headroom of §2.4 is measured here, on
  `bench-grammar-parse-big` and on a real module grammar (#9916 adds one).
- **E. The residue, then deletion.** Non-ratchet callee resumption, `LrCall`, call arguments,
  dynamic rule parameters, custom-HOW grammars. Also (D7 as sharpened): a compiled goal for every
  end at a position, so `:ov`/`:ex`, LTM lookahead fates and cursor token methods stop walking; the
  dynamic contexts that keep the engine out (`:my` lexicals and captures of an enclosing regex, a
  rule's dynamic declarations); and the walk's single-atom arm behind `CapAtom` and builtin calls,
  moved to shared leaf primitives. When the `regex-walk:` counter's `walked=` and `bridged=` read
  zero: delete the walk, the eager `Named` arm and the D6 mode, and close #7548.

## 5. Rejected alternatives

- **Keep shaving the walk.** #8450 did this: three items of unread work, about 31% between them. What is
  left is structural. Five candidate-generator layers and a `RegexCaptures` moved by value per atom cannot
  be shaved to ~18 ns a position (§2.3). Rejected on gain.
- **Lower regexes into the main VM's `OpCode`.** Rakudo compiles regexes to the same MoarVM bytecode
  as everything else, but MoarVM has native integer registers and a bstack op set; mutsu's `OpCode`
  is `Value`-oriented, is capped at 48 bytes (`opcode_size_guard`), and dispatches through
  `exec_one_dispatch`, whose per-op overhead is sized for `Value` work. A regex step is a position
  compare. A separate instruction set keeps both loops tight. Rejected on gain.
- **Closure compilation** (the token tree turned into nested Rust closures with continuations).
  Cheaper than the walk and less code than an instruction set, but backtracking state stays on the
  Rust stack. Resuming inside a non-ratchet subrule then needs continuations again, which is
  #7548's shape, and there is nothing for a later JIT to lower. Rejected on architecture.
- **Delegate the regular subset to `regex-automata` or `fancy-regex`.** Both are already in the
  dependency tree, but they index bytes or codepoints, not graphemes, and their `\w`, `\s`, case
  folding and Unicode classes are their own. Delegating would create a second definition of what an
  atom matches, the drift D4 forbids. On these shapes their PikeVM is also slower than a flat
  backtracker (§2.3). Rejected on semantics.
- **Wait for a measured loss against rakudo.** Mutsu no longer loses (§2.1). But AGENTS.md defines
  gain as moving toward a fast, maintainable interpreter, not as closing a gap. A measured 30x+
  headroom in the component that is 88% of a grammar parse, together with deleting the eager
  subrule arm and its six declined shapes, is that gain. Rejected.

## 6. Consequences

- ADR-0099 §4 Stages 2 and 3 are decided here. ADR-0007's "eventual ceiling" (a CPS→bytecode regex
  VM) becomes this ADR's machine, without CPS: continuations live on the explicit backtrack stack.
- ADR-0073's demand-driven candidate producers are replaced by choice points on the backtrack stack.
  What they guaranteed (a `{ … }` block runs once per end *entered*, not per end computed) holds for
  every call shape rather than the 71.5% #7548's survey measured as streamable.
- Two engines exist from Slice A until the end of Slice E. D6 is the price of that and is not
  optional.
- New tooling: `--dump-rx` prints a pattern's program, alongside `--dump-ast`.
- The regex benchmark suite gains the §2.3 failing-scan shapes, sized past the prefilter, so the
  walk's cost stays visible after this ADR.

## 7. Ordering against the open regex tickets

While both engines exist, two rules decide what happens to regex work outside the slices:

- **No performance work on walk internals.** It is deleted in Slice E, so the work would be
  discarded. A perf ticket whose cost lives in the walk is re-measured after the slice that
  replaces that code.
- **Correctness fixes in the walk are still made, with a test.** The test is the durable part. It
  carries into the compiled engine through D6 and the ordinary suites, so the fix is not lost when
  the walk is.

Applied to the tickets open on 2026-09-30:

| ticket | when | why |
|---|---|---|
| [#9916](https://github.com/tokuhirom/mutsu/issues/9916) benchmark suite (warm marginal cost, a real module grammar) | **before Slice A** | Slice A's kill criterion and Slice D's grammar figure need these benchmarks to exist first |
| [#10121](https://github.com/tokuhirom/mutsu/issues/10121) `<{ }>`, `** {n}`, `:my` compiled from AST per attempt | **before Slice C** | the fix gives each fragment a precompiled chunk, keyed at pattern level (ADR-0133). That survives the walk and is exactly what Slice C's call-out op invokes |
| [#9803](https://github.com/tokuhirom/mutsu/issues/9803) grammar attributes set in a method are lost on the Match | **in Slice D** | Rakudo's cursor *is* the grammar instance. D3's frame is where that instance belongs, so fixing it in the walk would thread state through code about to be deleted |
| [#9922](https://github.com/tokuhirom/mutsu/issues/9922) losing LTM branches may run side effects | **closed by Slice B** | `LtmAlt` enters branches in rank order and only on backtrack, so a losing branch's code runs only if the winner fails |
| [#7548](https://github.com/tokuhirom/mutsu/issues/7548) the streamed-subrule barrier | **closed by Slices D–E** | D3 makes every call demand-driven; widening the walk's eligibility analysis now is discarded work |
| [#9929](https://github.com/tokuhirom/mutsu/issues/9929) `.comb(Regex)` finds every match up front | **after Slice A** | a resumable scan is a saved `RxVm` state (start position plus registers), not a new mechanism in the walk |
| [#7576](https://github.com/tokuhirom/mutsu/issues/7576) YAMLish deeper documents ~2x | **re-measure after Slice D** | its remaining cost is grammar walk cost |
| [#10225](https://github.com/tokuhirom/mutsu/issues/10225) fate of `:P5` | independent | `:P5` runs on pcre2, not on the walk. It is outside D7's "one engine", and either decision leaves this ADR unchanged. Resolved by [ADR-0138](0138-perl5-regex-adverb-removed.md): `:P5` and the pcre2 engine are removed |
| [#10215](https://github.com/tokuhirom/mutsu/issues/10215), [#10216](https://github.com/tokuhirom/mutsu/issues/10216) `$/` / `$0` after `.subst` | independent | `Match` publication in the `.subst` layer, which survives |
| [#8033](https://github.com/tokuhirom/mutsu/issues/8033) RakuAST regex node tree | independent | front end: it produces the pattern this ADR compiles |
| [#10021](https://github.com/tokuhirom/mutsu/issues/10021) `Word_Break` emoji modifiers | independent | Unicode property data, consumed unchanged through D4 |
| [#9494](https://github.com/tokuhirom/mutsu/issues/9494) `Services::PortMapping` load time | independent until profiled | the cost is not yet attributed; if a profile puts it in the walk, the first rule applies |

## 8. Implementation status

Slice issues: A #10251, B #10252, C #10253, D #10254, E #10255.

**Slice A, first part (#10251): landed.** The engine lives in `src/runtime/regex/rx/`:
`rx_compile.rs` (the compiler), `rx_vm.rs` (the loop and the entry point), `rx_atom.rs` (atom
tests) and `rx_diff.rs` (D6). It is consulted from the walk's first-match chokepoint,
`regex_match_end_from_caps_in_pkg`, and so answers `~~`, the prefiltered scan, `:g`, `.subst`,
`.split` and every other caller of that function. It compiles:

- one-grapheme atoms and the zero-width assertions;
- `[ … ]`, and `( … )` whose body captures nothing;
- `$<x>=` / `$N=` aliases on non-quantified tokens;
- greedy, frugal, ratcheted and counted quantifiers over non-nullable bodies that capture
  nothing.

A greedy or ratcheted quantifier over a single one-grapheme atom is one `AtomRun` op. It scans the
iterations up front and gives them back from a position list.

`rx_atom.rs` adds an ASCII fast path that D4 allows. Each atom's acceptance of every printable
ASCII character is probed once, *through* `match_consuming_atom`, and consulted only where the
grapheme is provably that one character.

Everything else declines per pattern. The reason is reported on `MUTSU_VM_STATS`'s `regex-vm:`
line. `MUTSU_RX_VM=off` routes every pattern back to the walk. D6's `MUTSU_RX_DIFF=1` agreed with
the walk on every file of `t/regex/`, `t/grammar/` and the whitelisted `roast/S05-*`.
`tests/regex_vm_differential.rs` runs a corpus both ways in `cargo test`.

Kill criterion (release, 640 KB, best of 3):

| row | walk | compiled | ratio |
|---|---:|---:|---:|
| failing `\w+ \s \d ** 6` | 370 ms | 45 ms | 8.2x |
| failing `[ \w+ \s ] ** 3 \d ** 6` | 1,330 ms | 91 ms | 14.6x |

Both rows clear the 5x bar. `bench-regex-scan-walk`'s warm section goes from 0.58 s to 0.075 s.
That file adds a `:g` capture scan, which includes `Match` construction and improves 2.4x on its
own.

**Slice A, second part: landed.**

- The position-only matcher (`regex_match_nocap.rs`, used by `.comb` without captures, by
  `find_first` and by the walk's group probes) consults the compiled engine first. That fixed a
  bug on the way: the matcher ignored `:r`, so `"aaax bbx".comb(/ :r \w+ 'x' /)` found two
  matches where rakudo finds none.
- Quantified capture groups one level deep compile (`(\w)+`, `[ (\w) (\d) ]+`). They mark
  their names quantified before the first iteration and fold at the loop's exit through the
  walk's own `fold_quantified`.
- So do captures and aliases under `?`. The empty arm replays `walk_zero_or_one_zero_arm`'s
  slot reservation.

A `MUTSU_VM_STATS` sweep over the first half of `t/` puts the patterns still declined at:
`subrule` 191 (Slice D), `code` 49 (C), `alternation` 41 (B), `ignorecase` 23 (B), and a tail of
Slice A shapes (`sequential-alternation` 11, `composite-class` 8, `nullable-loop` 7,
`separator` 4).

**Slice A, third part: `||` landed.** Sequential alternation compiles in
`walk_seq_alternation`'s order: every end of branch *k* is tried against the rest of the pattern
before branch *k+1* is entered. Each branch ends with an `AltTail` op that merges what
`alternation_branch_delta` adds: padding to the widest branch, and the list-valued names marked
quantified. The padding is built by `alternation_tail_delta`, a helper the walk shares. Under
ratchet the alternation commits to the first matching branch's first end. Two shapes still
decline: a ratcheted alternation with a nullable branch before the last, and a numbered alias
inside a branch.

The comparison exposed two walk bugs, both now fixed against rakudo:

- The quantified-alternation padding flag leaked into the continuation after a
  `walk_quant_group_candidates` loop.
- `**` over an alternation never backtracked into an earlier iteration's branch choice.

**Slice A, fourth part: nullable loop bodies landed.** A quantifier whose body can match empty
ends each iteration with a `ZeroIter` guard. The guard calls the walk's `zero_width_iter_counts`,
and rejecting an iteration backtracks into the body's other candidates, as the walk's group DFS
does. The walk's chain takes first candidates only, so a nullable body compiles when it is a
DFS shape (a group, or anything containing an alternation), when it is ratcheted, or when it is a
single-candidate assertion.

**Slice A, fifth part: `CompositeClass` landed.** It is a one-grapheme atom tested by
`match_consuming_atom`. A class with a named item can fall back to a grammar token, which depends
on the package and the real subject, so it skips the per-program ASCII probe table.

A full `MUTSU_VM_STATS` sweep (all of `t/` plus the roast whitelist, after `||`) counted 4937
compiled and 2281 declined patterns. Later slices account for most of the declines: `subrule`
494, `code` 357, `alternation` 347, `ignorecase` 253 and `lookaround` 220. The old catch-all
`other-atom` (216) is now split into `isolated-group`, `interpolation`, `ws-rule`, `goal-match`
and `conjunction`, which also belong to later slices.

The same part compiles `%` and `%%` over a capture-free atom and separator, in the order of
`for_each_separated_candidate` (non-ratchet) and `match_separated_quantifier_ratchet`. Two new
ops support it: `Advanced` for the per-step progress guard, and `AtLeast` for the minimum count.
Frugal separated quantifiers now use the native separator parser, shortest-first candidates in the
walk, and frugal `Split`/`Repeat` priorities in the compiled engine (#10306). The LTM text
expansion leaves non-sigspace frugal separators intact so neither parser loses the modifier.
Sigspace separated quantifiers still use the LTM text expansion; its frugal ordering and
per-iteration whitespace remain #10339.

**Slice A, sixth part: the rest of the capture language landed.** Slice A's atoms are now
complete.

- **Capture levels.** A `( … )` whose body captures opens a capture level of its own
  (`rx_levels.rs`), so its captures number from zero and become the group's sub-Match through
  the walk's `capture_group_delta`. Backtracking can resume inside a group that has already
  closed. So every change to the level stack is journaled: a store edit keeps its level and
  trail mark, and a close keeps the popped store whole. A choice point records one journal
  length. Nested quantified captures (`((a)(b))+`, `[ (a)* ]+`) then need nothing new: each
  iteration pushes its slots and the loop folds them with `fold_quantified`.
- **Backreferences and `<(` / `)>`** are one op, `CapAtom`. It calls the walk's own
  single-candidate matcher and merges the delta it returns. A capture group whose body holds a
  backreference also opens a level, because a group is its own capture scope
  (`/ $<x>=(\w) ( $<x> ) /` fails in rakudo).
- **Quantified aliases** (`$<x>=(\d)+`, `$<x>=[a]+`) apply the alias once per iteration, over
  that iteration's span, as `grow_one_iter` does.
- **Captures under `%` / `%%`.** Each atom and each separator matches in a level of its own,
  and the closed levels are collected in match order. At the quantifier's end, `SepEmit` folds
  them side by side through `separated_capture_delta`, a helper the walk's three separated
  paths now share. A separated quantifier whose atom or separator holds a backreference still
  declines (`separator-backref`), because the walk matches each iteration against the captures
  folded so far. An aliased one declines too (`separator-alias`).

The comparison found two more walk bugs, both fixed against rakudo:

- A backreference inside `[ … ]` or a `||` branch numbered from the group's own captures. So
  `/ (a) [ (b) $0 ] /` compared `$0` against `b`. The walk now continues the enclosing level's
  numbering (`RegexCaptures::backref_positional`).
- `alternation_branch_delta` dropped a `<(` / `)>` marker set inside a `|` or `||` branch, so
  `/ x [ c || a <( b ] /` matched `xab` instead of `b`.

The same part compiles `@<x>=` and secondary-name aliases, `<?same>` and `<at(N)>`, and
reports `<~~>` as `recurse-self` instead of `other-atom`. `scripts/rx-decline-survey.sh` now sums
the D5 counter over all of `t/` and the roast whitelist. Its totals went from 5,154 compiled and
2,149 declined to 5,290 and 2,007. The declines are now almost all later slices: `subrule` 521
(D), `code` 375 (C), `alternation` 375, `ignorecase` 255, `lookaround` 226, `ignoremark` 22 and
`conjunction` 20 (B). A small tail of Slice A shapes remains, each of them a walk behavior the
compiled engine would have to copy first: `nullable-loop` 17 (a non-DFS nullable body whose
first candidate the walk's chain keeps), `separator-frugal` 3 (#10306), `frugal-ratchet` 3,
`seqalt-nullable-ratchet` 2 and `empty-range` 2. Slice E's deletion criterion covers them.

The unanchored scan loop still runs outside the VM, one `rx_run` per start position. Moving it
in is a performance change, not a language one, and it is tracked as
[#10315](https://github.com/tokuhirom/mutsu/issues/10315) (`todo:perf`).

**Slice B, first part (#10252): `|` landed.** One `LtmAlt` op per alternation ranks the branches
at the current position with the walk's own `ltm_branch_rank_key` (`rx_ltm.rs`). It pushes the
lower-ranked branches as choice points, next-best on top, and enters the best one. A branch is
therefore entered only after every branch ranked above it has failed against the rest of the
pattern, which is what #9922 asked for. That issue is closed with
`t/regex/regex-ltm-losing-branch-code.t`. Its patterns carry code blocks, so the walk still runs
them until Slice C; the walk's `drive_alternation_candidates` already has the same order. Branch
captures merge through the `||` path's `AltTail`. Under ratchet, a cut after each branch commits
to the first branch that matches and to its first end. A numbered alias inside a branch still
declines (`alt-numbered-alias`), as it does for `||`.

**Slice B, second part: lookahead and lookbehind landed.** A lookaround is a `CapAtom`. The op
calls the walk's own lookaround test, which matches the body through
`regex_match_end_from_caps_in_pkg`. That function answers from the body's own compiled program, so
a lookaround compiles only when its body does. Otherwise the pattern declines with
`lookaround-body`, and no body drops back to the walk in mid-program (D5). A lookaround body
therefore runs as a nested `rx_run`. The per-run scratch is now a small pool, so a nested run
reuses its own warm scratch instead of allocating one per test.

**Slice B, third part: `:i` and `:m` landed.**

- **`:i`.** Every atom records its pattern level's `:i` and passes it to the walk's own
  tests, as the walk passes `ctx.pattern.ignore_case`. A scoped `[:i …]` therefore covers its
  body only. The ASCII probe runs under the same flag. An atom whose probe ever consumes more
  than the one character (a fold that expands) gets no table and takes the full test. A
  whole-pattern `:i` over a multi-character fold still matches on the case-folded subject. Its
  folded pattern is now memoized in `PatternDerived` (`casefold_pattern_cached`), so its program
  is compiled once rather than on every match.
- **`:m`.** A whole-pattern `:m` runs the mark-stripped pattern's compiled program over the
  subject's stripped view. It is mapped back by `ignoremark_on_target`, the walk's own remapping,
  moved into `regex_ignoremark.rs` so both engines share it. A scoped `[:m …]` inside a larger
  pattern still declines (`ignoremark`), since it would need that remapping at a group boundary.

**Slice B, fourth part: `&` landed; Slice B is complete.** The first branch of a conjunction
runs inline, in a capture level of its own. A `ConjTail` op then asks every other branch to
match exactly the same span with a nested run of its own program. `rx_run` takes an optional
required end for this: the first match in priority order that ends there, which is how the
walk's `regex_match_branch_ending_at` picks one from the full end list. All branches' captures
merge with the walk's `merge_regex_captures`. A conjunction declines when a branch holds a
backreference (`conjunction-backref`), because the branch levels would hide the enclosing
captures, or when a later branch does not compile (`conjunction-branch`). As a loop body in
the walk's first-candidate chain, the conjunction is cut after each iteration.

The position-only matcher (`.comb`) took the longest branch end instead of requiring a common
span, so `"ab cd".comb(/ \w+ & <[a..c]>+ /)` found `cd`. It now asks the capture matcher. Found
on the way: `:r` does not reach conjunction branches (#10353), and `.comb` with `:m` misses
matches (#10352). Both are pre-existing, and both engines agree on them.

**Slice C, first part (#10253): `{ }`, `<?{ }>`, `<!{ }>` and `:my` landed.** A code atom is a
call-out. The walk's `CodeAssertion` and `VarDecl` arms moved into one leaf, `regex_code_atom.rs`
(`regex_code_atom`, `regex_var_decl_atom`). The walk's single-candidate matcher now calls them, and
so do the compiled engine's `Code` and `VarDecl` ops, which pass the innermost capture level as the
code's view and merge the delta that comes back (the `:my` lexicals written, the `make` value).
Nothing is precomputed, so a code atom runs only where the cursor reaches it, in the order
backtracking reaches it. The body's compile is the cached one of ADR-0133 and #10121
(`eval_regex_inline_code`), so there is no per-attempt AST compile and no new `Interpreter`.

- **The position-only matcher** (`.comb` without captures, the walk's group probes) treats a code
  atom as an inert zero-width pass. A program that runs code (`RxProgram::has_code`, which includes
  a lookaround body's) therefore declines there and keeps that matcher.
- **A capture group whose body holds code** opens a capture level of its own, as one whose body
  captures or back-references does. `$/` inside `( … { … } )` is the group's own match so far, and
  `$0` its first capture.
- **Differential mode compares the code.** Running the walk a second time over a pattern with code
  would run the user's code twice. So the compiled run *records* each invocation (code text,
  position, the captures visible to it, the result it gave) and the walk *replays* them
  (`rx_diff.rs`): the walk's n-th invocation must be the recorded n-th one, and it is answered from
  the record instead of being run. Nested runs (a lookaround body) share the record, and only the
  outermost run compares. This is the order comparison D6 asks for, and it keeps
  `MUTSU_RX_DIFF=1` free of doubled side effects.
- **A bug in the walk, found by that comparison.** A non-capturing group, a `|` / `||` branch or a
  quantified group gave the walk a capture scope of its own, so `{ say $/.Str }` inside
  `/ a [ b { … } ] c /` printed `b` where rakudo prints `ab`, and `$0` of the enclosing regex was
  invisible to the block (`/ (a) [ b { say $0 } ] c /` printed `Nil`). A same-scope sub-pattern that
  holds code now publishes the enclosing level's captures and match start for the nested walk, the
  way one that holds a backreference already did (`atom_contains_code`, `OuterBackrefCaps::match_from`;
  the parser's `note_regex_code_lowered` keeps the cost at zero for a process with no code in a
  regex). A capture group and a lookaround still get a scope of their own, as in rakudo.
  `t/regex/match/regex-code-atom-capture-scope.t` pins the rakudo values.
- **A second walk bug, in the published scope's lifetime.** A sub-pattern publishes its level's
  captures for ITS nested walks, but the rest of the enclosing pattern runs inside its dynamic
  extent (candidates are streamed), so a subrule called after a `[ … { … } ]` group inherited the
  group's match start: `$/` in its code spanned `abc12` where rakudo gives `12`. Backreferences had the
  same latent leak. A subrule call, a capture group, a lookaround and code that may run a match now
  arm a barrier whenever a scope is published (`arm_subrule_barrier`, `atom_starts_own_regex`;
  `t/grammar/grammar-subrule-match-start-after-code-group.t`). `t/grammar/ipv6-mapped-dotted-decimal.t`
  and `t/modules/batteries/xml-battery.t` caught it.
- **Two shapes still decline**, for the reason `separator-backref` does: code reads the enclosing
  captures, and these shapes hide them. Code inside a `%` / `%%` quantifier (`separator-code`) sees
  the iterations folded so far in the walk (Net::Whois's `$/[*-1][*-1] < 256` octet check, pinned by
  `t/regex/match/regex-separated-quantifier-code-assertion-captures.t`), where the compiled form
  matches each iteration in a level of its own. Code inside a `&` branch (`conjunction-code`) runs in
  a level or a nested run of its own.

`scripts/rx-decline-survey.sh` (all of `t/` and the roast whitelist) puts `code` at 83 (from 406),
`separator-code` at 7 and `conjunction-code` at 2; compiled patterns went from 5,981 to 6,319.
D6 agreed with the walk on all of `t/` and the roast whitelist (6,933 files), once the two declines above were in. The
remaining `code` declines are the parts of Slice C not yet landed: `<{ … }>` closure interpolation and
`** {n}` (the count is evaluated when the quantifier is reached, so it needs run-time bounds on the
loop ops), then `<$var>` / `<@var>` / `$( … )` interpolation.

**Slice C, second part (#10253): the interpolation atoms landed.**

- **`<$rx>` / `$rx`** (a Regex value spliced in, `CaptureIsolatedGroup`) compiles to
  `OpenIsolated … DropCapture`: the body runs in a capture level of its own and the level's
  captures are dropped when it closes, which is `GroupShape::Isolated`. The level inherits no
  `:my` lexicals, as the walk's barrier for a regex of its own arms none. Under ratchet the group
  commits to its first end, as a capturing group does. A value that closed over its own scope
  (`CaptureIsolatedGroupScoped`) still declines (`isolated-group-scoped`): installing that scope
  around a body the program can backtrack into needs an enter/exit pair of ops.
- **`$x` of an in-regex `:my` lexical** (`VarInterp`) and **`<{ … }>`** (`ClosureInterpolation`)
  compile to `CapAtom`, which calls the walk's own single-candidate arm. The closure arm moved into
  the shared leaf (`regex_closure_interp_atom`) and goes through `rx_code_call`, so D6 records and
  replays it. Both mark the program as one the position-only matcher must not run (`has_code`):
  `<{ … }>` runs code, and `$x` reads lexicals that matcher does not have.
- **Nested levels read the enclosing `:my` lexicals.** The walk publishes them to an inline
  sub-pattern through the vars seed. A compiled capture group, separated-quantifier iteration or
  conjunction branch opens a level that starts with the enclosing level's lexicals (shared, not
  copied). The first part's sweeps missed it, because no `$x` was compiled yet; the 15 test files
  that landed on `main` since then found it
  (`t/regex/regex-lookaround-bound-param.t`: `$ni` read inside a `%` quantifier's atom). A
  conjunction branch that reads a lexical declines with `conjunction-code`, as one that runs code does.
- **D6's record is order-exact.** A code block can match a regex of its own, which holds code of
  its own. The record used to push an invocation after its run, so the inner events preceded the outer
  one and the replay saw them out of call order. An event is now reserved before the run and
  keeps how many events the run produced, so a replay answers the invocation from the record and skips those.

Survey: `isolated-group` 90 → 0 (17 scoped values decline under their own reason), `interpolation`
55 → `code-interp` 22 + `qq-interp` 9, `code` 83 → `repeat-code` 42; compiled patterns 6,319 → 6,497.
What is left of Slice C is `** {n}` (run-time loop bounds), `$( … )` / `@( … )`
(`CodeInterp`, which yields several candidate ends) and `"…$x.meth()…"` (`QqInterp`), and the
declines named above.

Two walk bugs found on the way, which both engines share and D6 therefore cannot see, are filed:
`<{ … }>` merges the interpolated pattern's captures into the caller where rakudo discards them
(#10417), and `$/` in a `<{ … }>` body is empty where rakudo gives the match so far (#10418).

**Slice C, third part (#10253): `$( … )` / `@( … )`, `** { … }` and `"…"` thunks landed.**

- **`$( … )` / `@( … )`** (`CodeInterp`) compiles to `InterpEnds`. The walk asks for every end of
  the pattern the code yields up front (`regex_code_interp_ends`, with each end's capture delta), so the
  op does the same through the same function and enters the ends highest priority first. The rest wait
  on the backtrack stack as one choice point, `Choice::Cands`, which holds the list and how many are
  left and merges the next candidate's delta after the rewind. Under ratchet the atom commits to the
  first. The position-only matcher keeps its unrecorded copy of the call, since the compiled engine
  never stands in for it.
- **`** { … }`** takes its count from code evaluated where the quantifier is reached, before the
  names under it are marked. `RepeatCount` evaluates it through `regex_repeat_count`, the walk's own
  call, and stores the bounds in two registers. `RepeatDyn` is the `Repeat` loop op reading its bounds
  from them. A body that can match empty would need a `ZeroIter` guard built from static bounds, so it
  would decline (`repeat-code-nullable`); no pattern in `t/` or the roast whitelist hits it.
  A `**` with a separator (`% ','`) still declines (`repeat-code`, 4 patterns).
- **`QqInterp`** (a `"…"` atom whose interpolations a thunk resolved at rule entry) compiles to
  `CapAtom`: the result is read from the environment, so there is no code to run at match time.
- **D6's record is generic over the answer.** An invocation answers with a match, a list of candidate
  ends or a pair of bounds; `rx_code_call` takes any of them (`CodeValue`), so one record serves
  `{ }`, `$( )` and `** { }` alike.
- **Every code-bearing atom counts as code for scope seeding.** `$( … )`, `<{ … }>` and a `** { … }`
  token publish the enclosing level's captures and match start to the group they sit in, as `{ … }` does.
  The first run of the D6 sweep showed `$/` starting at the group inside `[ @(<a ab>) ]+`.

Survey: `code-interp` 22 → 0, `qq-interp` 9 → 0, `repeat-code` 42 → 4; compiled patterns 6,497 → 6,592.
D6 agreed with the walk on all 6,955 files.

**What is left of Slice C** are the declines that keep the walk: `isolated-group-scoped` (17: a spliced
Regex value that closed over its own scope), `separator-code` (7) and `conjunction-code` (2): the
compiled form hides the enclosing captures from code in a `%` quantifier or a `&` branch, and the
walk's `InlineCaptureScope` fold, which Net::Whois's octet check reads, has no compiled counterpart yet.

Three more walk bugs, which both engines share and D6 therefore cannot see, are filed: an aliased
atom under `** { … }` records one capture per iteration where rakudo records the whole span (#10444),
and the scalar `$( $re )` of a Regex value matches the literal text of its source (#10445), besides
#10417 and #10418 from the second part.

**Slice C, fourth part (#10456): code in a `%` quantifier and in a `&` branch landed.** An
iteration of a separated quantifier and a conjunction branch still match in a level of their own,
but when they hold code that level is an *inline* one (`OpenSepIter`, `OpenInline`;
`rx_levels::inline_level_caps`). It starts with the enclosing level's whole view
(`inline_capture_view`, flattened) linked as its outer captures, and with the enclosing match
start and `:my` lexicals. A separated quantifier's iteration also gets the iterations collected so
far, folded (`rx_sep_fold`, the walk's `SepChainWalk::assemble`), with its own captures folded into
the atom slots, or into the separator slots for a separator (`merge_positional`). A conjunction's
later branches run in nested runs seeded with the same view plus the earlier branches' captures
(`ConjTail { seeded }`, `rx_run_seeded`). The levels close as before. The outer link is stripped,
so nothing of the view travels out with the captures.

The values are rakudo's (`t/regex/syntax/regex-separated-and-conjunction-code-view.t`). Comparing
the engines found four walk bugs, fixed in the walk the same way (`regex_match_sep_view.rs`,
`arm_conjunction_branch_seed`):

- The fold start was relative to the walk level's own captures, so a quantifier in a `[ … ]` after
  a capture folded into the earlier capture's slot (`inline_visible_positional_len`).
- An atom did not see the separator just before it.
- A separator's code saw nothing of the chain, in either the backtracking or the ratcheted scan
  (`regex_match_sep_ratchet.rs`, split out of `regex_match_sep.rs`).
- A conjunction's later branch ran under the barrier the first branch's last code atom armed, so it
  saw neither the enclosing captures nor the earlier branches'.

Two differences from rakudo that both engines share are filed: zero iterations drop the quantifier's
positional slot (#10534), and a nested separated quantifier's fold sits beside the outer slot
instead of in it (#10535, settled by the fifth part below).

Survey (`scripts/rx-decline-survey.sh`, all of `t/` and the roast whitelist): `separator-code` 7 → 0
and `conjunction-code` 2 → 0, with 9,900 patterns compiled and 113 declined. D6 agreed with the
walk on every file of `t/regex/`, `t/grammar/` and the whitelisted `roast/S05-*` (611 files).

**Slice C, fifth part (#10535): a capture group under nested quantifiers is one slot.** In
`[ [ (\d) { … } ] +% '.' ] +% ';'` rakudo has one `$0`: the groups around `(\d)` do not capture, so every
iteration of both quantifiers lands in the same flat list, in what code sees mid-match and in the match's
value. mutsu had three defects, which share the cause that a quantifier's fold knew nothing of the
quantifier around it. The repro in #10535 only showed the first.

- **The view.** Code in an inner iteration read the outer view with the inner fold *appended* as a slot of
  its own (`[]|[1,2]` where rakudo has `[1,2]`). The enclosing level of a nested quantifier is itself an
  iteration, whose own captures fold into the outer iteration's slots (`merge_positional`), and the
  inner quantifier's captures *are* those captures. The view now composes the folds from the outermost
  link in (`regex_backref_scope.rs`): `ViewFold` places one level's own captures in the view it reads,
  folding the first `stride` of them into the slots from `start` and appending the rest, and
  `OuterBackrefCaps::append_captures` applies each link's *parent's* range to the link's own captures
  (it used to ignore it). `merge_positional` counts slots of that merged view, so the start an iteration
  gets (`sep_iteration_slot` in the walk, `inline_level_caps` in the compiled engine, through
  `inline_view_slot` / `inline_view_fold`) is where the quantifier's slots *land*, not where they
  would follow. The compiled engine keeps its flat single link; its `extra` (the iterations folded so
  far) now folds through the same `ViewFold` instead of being appended.
- **The value.** An outer fold (`fold_quantified_captures`, `append_separated_captures`) listed one entry
  per iteration slot, the last entry only for a slot an inner quantifier had already folded, so
  `"1.2;3.4" ~~ / [ [ (\d) ] +% '.' ] +% ';' /` gave `[2,4]` and `[ [ (\d) ]+ ]+` on `1234` gave `[4]`.
  A slot that is already a list now contributes each of its entries (`PosSlot::push_entries_to`; the
  folded slot is built by `PosSlot::folded`), in all three folds and in the view's. A capturing group's
  own quantified sub-captures sit inside its Match, not in the iteration's slots, so they are not
  flattened (`( (\d) +% '.' ) +% ';'` keeps a list per outer iteration). An alternation's empty-list
  padding slot, which used to add a bogus `(0, 0)` entry, now adds none.
- **The stride.** `count_pattern_capture_groups` and `pattern_capture_group_list_flags` ignored the
  capture groups of a nested separated token's *separator*, though such a token takes the atom's slots
  and then the separator's. The outer quantifier's stride was one short, so
  `[ [ (\d) ] +% (<[.]>) ] +% ';'` lost the separator slot and `[ (\d) +% (<[.]>) ]+` folded the
  separator's entries into the atom's list. `separator_stride` is now the same memoized count.

The values are rakudo's (`t/regex/match/regex-nested-quantifier-capture-fold.t`, 30 cases, verified under
`raku`), and `tests/regex_vm_differential.rs` pins that both engines and D6 agree on them. D6 agreed with the
walk on every file of `t/regex/` and `t/grammar/` (534 files). Three things found on the way are out of
scope and filed: code in an *unseparated* quantifier's iteration sees the raw per-iteration entries, not
the folded slot (`[ (\d) { … } ]+` shows `1|2|3`, in both engines, so D6 cannot see it, and nested
plain-in-separated makes the engines disagree; #10597); an aliased group `$<x>=(\d)` under any
quantifier takes a positional slot rakudo does not (#10598); and the walk folds a `[ … ]` group after
a capture inside a separated atom into the wrong slot, which the compiled engine does right (#10599).

**Slice D, first part (#10254): subrule calls landed.**

A `<subrule>` call compiles to one `Call` op carrying its `NamedAtom`; the program stays a pure
function of the pattern, and the call is resolved when it is reached (`rx_call.rs`). Four things can
come out of that resolution:

- **A plain rule with a program** runs as a frame in the run's own loop. The callee gets a register
  window in one arena, a capture level of its own (`levels.open(pos, false)`, a regex of its own: it
  inherits no `:my` lexical), and a persistent `Rc<Frame>` linked to its caller. Its choice points go
  on the **same** backtrack stack. When it returns, its level closes into the callee's captures,
  which are filed as the subrule's Match by the walk's own builder
  (`build_named_candidate_from_inner`, split out of `build_named_candidates_from_inner`) and merged
  into the caller's level.
- **A proto whose candidates all have programs** ranks them at the call with the walk's own LTM
  measurement (`rx_rank_proto`, ADR-0046) and enters the first that matches as a frame with the
  `:sym<…>` on its Match. A `Choice::Proto` entry holds the next-ranked candidates; the first that
  returns commits the call (the proto's greedy first end only, as the eager arm does), which drops
  the entry.
- **A call with no rule behind it** (`<.ws>` of a `rule`, `<wb>`, `<alpha>`, …) asks the walk's
  single-candidate arm directly. A grammar method of that name bridges instead.
- **Anything else** (arguments, `$*` parameters, wrapped tokens, a custom HOW, a left-recursive
  rule, a rule the compiler declined, `:m`, an inherited `:i`) is the bridge of D5: the walk's eager
  producer (`regex_match_atom_all_with_capture_opts`) computes the callee's ends and they are
  entered highest priority first through one `Choice::Cands`. That is exactly what the walk does for
  those shapes today, so the bridge changes nothing about them.

The frame shape is the shape `drive_named_subrule_candidates` streams, with one difference that the
compiled engine allows: a rule that calls itself is fine as long as it cannot re-enter *at the same
position* (`subrule_cannot_left_reenter`). A frame, unlike the walk's stream, needs no
left-recursion activation to stay sound, so the whole JSON-style grammar (`value` → `array` →
`value`) runs frames. A live walk activation of the same name still sends the call to the walk.

Ratchet is a cut. `commit` (the call's token is ratcheted, or the call is a proto's) truncates the
backtrack stack to its height at the call when the callee returns, so nothing ever resumes in the
callee. A call that is not committed leaves the callee's choice points where they are, and **every
choice point records its frame** (`FMark`, a side stack that only exists for choice points pushed
while a callee is live): a failure after the callee returned resumes inside it with its callers
behind it, Rakudo's bstack model. That is the "non-ratchet callee resumption" Slice E listed, and it
costs nothing extra, so it landed here. Two returns at the same end are one candidate, as in the
walk (`Frame::seen`).

A return that leaves no choice point in the callee forgets the callee's journal, register trail,
window and `AtomRun` ends (`settled`), and the undo trails restart whenever the stack is empty, so a
run of ratcheted calls stays flat in memory.

Quantified calls: `<x>*`, `<x>+` and `** n` call the walk's single-candidate arm per iteration
(`CapAtom`), one first end each, as `grow_one_iter` does. A ratcheted `*` / `+` first offers the
walk's possessive scan (`NamedRun`, over the leaf `regex_named_ratchet_run` that
`walk_ratchet_fast_paths` now calls too, so there is one implementation).

`Grammar.parse` is answered by a compiled run (`rx_try_ends_until_full`, `Goal::UntilFull`): it
collects the ends in priority order up to the first that covers the subject, which is the list
`regex_match_ends_stop_at_full` returns. `~` goal matches compile (`GoalEnd`, `GoalOk`,
`GoalFail`): the inner pattern and the goal each match in a level of their own, the goal's captures
merge first, and a goal that matches nowhere after an end of the inner pattern records the failure
for the "expected goal" report from a handler sitting below the goal's choice points.

D6 changed in three ways. The walk's replay is now the walk alone (a nested pattern it matches no
longer answers from the compiled engine), `node_span` describes a Match tree recursively, and the
replay runs with the reduce log set aside (`isolate_reduced_log`), because the walk re-logged every
subrule the compiled run had already logged and each action ran twice. Differential mode declines
a match when a wrapped token or a custom HOW is live: that is user code the record cannot replay.

Correctness comes from three sweeps, all in differential mode (`MUTSU_RX_DIFF=1`, D6): every file of `t/` (5,535)
and every whitelisted roast file (1,426) ran with no disagreement between the compiled engine and the walk, and
`tests/regex_vm_differential.rs` gained eight cases (non-ratchet resumption, end dedup, proto, quantified calls, `~`,
recursion and left recursion, captures under groups and aliases). `t/grammar/grammar-subrule-call-frames.t` pins the
rakudo values (verified against `raku`) for the shapes that rakudo supports. One walk quirk both engines share, so D6
cannot see it, is filed: the walk deduplicates a callee's ends where rakudo enters each path
([#10489](https://github.com/tokuhirom/mutsu/issues/10489)).

Two things the sweeps found: an iteration of a `*` / `+` over a call runs the call's action when an action-driven parse
reads a `$*` variable (`maybe_run_reduce_time_dynvar_action`, Template::Mustache's delimiter change,
`t/grammar/grammar-reduce-time-dynvar.t`), which the walk does in `grow_one_iter` and the compiled loop now does after
each committed iteration (`ReduceAction`); and the first version of the loop cost the Slice A scan rows 20-30%. The run
loop is now compiled twice (`rx_run_in::<FRAMES>`, chosen by `RxProgram::has_call`), a choice point stays four words
(the frame state of one pushed inside a callee lives in a side stack, `FMark`), and the one wide variant is boxed.

**Grammar headroom (§2.4), measured.** Release, warm, best of five, this box, rakudo 2026.07 on the same files:

| workload | before | after | rakudo |
|---|---:|---:|---:|
| `bench-grammar-parse-big` (10,453 chars), wall | 32.5 ms | 25.0 ms (-23%) | 74 ms |
| the same, callgrind, one parse | 241.1 M instr | 179.7 M (-25.5%) | |
| the same, per document character (net of the 12.0 M start-up) | 21,900 | 16,040 | |
| `bench-grammar-json-tiny` (JSON::Tiny::Grammar, 31,590 chars), wall | 45.0 ms | 40.0 ms (-11%) | 92 ms |
| `benchmarks/bench-yaml-parse.raku` shape, 120 rows, wall (#7576) | 0.60-0.71 s | 0.63-0.65 s | |
| §2.3 scan row `[ \w+ \s ] ** 3 \d ** 6`, wall / callgrind (40 KB) | 138 ms / 132.6 M | 138 ms / 138.4 M (+4.4%) | |

So the compiled form does **not** buy a grammar parse anything like the 32-40x of §2.3: the whole of
`bench-grammar-parse-big` now runs in one compiled loop (`regex-vm: compiled=15 declined=0`) and it is a quarter faster,
not an order of magnitude. ADR §2.4 expected exactly this, and the profile says where the rest is:

- **LTM ranking of proto candidates**: 22.5% inclusive (`ltm_measure`, 35,528 calls for about 5,000 proto calls: one
  NFA run per candidate, seven candidates in `value`), of which 19.6 M is the one-time NFA build.
- **Allocation**: 186,000 allocator calls per parse (225,000 before), about 21% of the instructions with `memcpy`. They
  are the match tree itself: a `RegexCaptures` per callee, its named map, the `CapNode`, the capture lists.
- **The loop itself** is 10% (`rx_run_in::<true>`) and `CapStore::merge_delta` 9% inclusive.

The §2.3 premise (a flat program is 30x cheaper than the walk per position) holds for patterns whose cost is the
walk; a grammar's cost is per subrule: resolution, ranking, the Match tree. Those are what is left to remove, and they
are filed as `todo:perf` issues with their own goals rather than closed here
([#10487](https://github.com/tokuhirom/mutsu/issues/10487) one NFA run per proto call,
[#10488](https://github.com/tokuhirom/mutsu/issues/10488) the allocations of the Match tree). On YAMLish the compiled engine changes
nothing measurable (#7576): its cost was never the engine. The allocations are decided by
[ADR-10488](10488-capture-levels-are-written-in-place.md): capture levels are written in place, not
assembled from per-call deltas.

What is left of Slice D after that: the walk's `drive_named_subrule_candidates` and eager `Named` arm, which Slice E
deletes once the bridge's shapes are compiled (`LrCall`, call arguments, `$*` parameters, wrapped tokens, a scoped
`[:m …]`, which keeps `JSON::Tiny`'s string token on the walk). The survey over `t/grammar` and `t/regex` puts compiled
patterns at 97.2% (3,594 of 3,696), with no `subrule` decline left; the declines are `frugal-ratchet` 20,
`nullable-loop` 19, `ignoremark` 18, `isolated-group-scoped` 17 and a tail.

#### The cursor is the grammar instance ([#9803](https://github.com/tokuhirom/mutsu/issues/9803))

In Rakudo a rule invocation runs on a cursor that *is* an instance of the grammar. A method a rule calls as a subrule
(`<.acc>`) writes `$!attr` of that cursor, and when the rule returns, the cursor is its Match. Measured against
rakudo 2026.07: `G.parse("a")<t>.inv` is `True` for `token t { a <.acc> }` with `method acc { $!inv = True; self }`
while `G.parse("a").inv` is `Any`; two matches of one token each start from the uninitialised attribute (`$!n++` gives
1 and 1); calls from one invocation accumulate (3 for three `<.bump>`); a cursor is created, not built, so every
declared attribute reads as its uninitialised value (`Any`, `Int`, `[]`, `{}`) and a `= default` is not applied.

- **The instance lives where the invocation does.** A `Frame` (and the run's own root) holds
  `cursor: RefCell<Option<Value>>`, filled the first time a call in that frame runs a grammar method (`rx_cursor_of`,
  a `CREATE` of the grammar: every declared attribute present, uninitialised, because the method's write-back only
  updates keys already on the instance). The bridged `Call` publishes it in `Interpreter::rx_cursor` for that one call,
  `try_regex_subrule_as_method` takes it as the method's invocant, and the frame's return files it on the callee's
  captures (`RareCaps::cursor`, then `CapChildren::cursor`). A rule that never calls a method creates nothing.
- **The walk gets the same scope** (`Interpreter::walk_cursors`, one entry per rule invocation in flight) around every
  place it evaluates a rule body and files the result as that rule's Match: the eager `subrule_candidate_ends`, the
  streamed arm (lifted across the continuation, which is the caller's pattern), the ratcheted `<x>*` scan, the
  single-candidate arm and the start rule. This is not optional while the bridge exists: a rule whose left cone the
  call graph cannot name (a `<-crlf>` class, a method at a nullable-left position) is handed to the walk, and the
  documented `HTTPRequest` example hits exactly that for `field`. It goes with the walk in Slice E; the
  `MUTSU_RX_VM=off` run of `t/grammar/grammar-cursor-attributes.t` keeps it honest until then.
- **The Match** materializes the instance's attributes next to its own (`match_lazy`), and the generated accessor of an
  unset declared attribute on a grammar cursor answers the uninitialised value instead of `Nil`.

Not covered, and filed: a method inherited from a parent grammar is not found as a subrule of a derived grammar
([#10508](https://github.com/tokuhirom/mutsu/issues/10508): `user_method_overloads` is per declaring class, so `B2 is B1`
fails to parse through `B1`'s `<.acc>`, before and after this change), and `self` inside a token's `{ … }` code block,
which is the cursor in raku and dies in mutsu ([#10509](https://github.com/tokuhirom/mutsu/issues/10509)).

### Slice E, first part: every use of the walk, counted ([#10255](https://github.com/tokuhirom/mutsu/issues/10255))

Slice E opens by making the whole residue visible. `MUTSU_VM_STATS` prints a second line next to
`regex-vm:`, counting *events* rather than patterns:

```
[mutsu vm-stats] regex-walk: walked=N (reason=n …) bridged=M (reason=n …) leaf=L (reason=n …)
```

- **`walked`**: a whole match the walk answered. `declined` (the pattern has no program),
  `position-only-code` (the position-only matcher meets a program that runs code), `context:*` (the
  first condition of `rx_context_allows` that failed: `vm-off`, `ltm-declarative`,
  `rule-dynvar-decls`, `inline-regex-vars`, `inline-capture-scope`, `inline-outer-seed`, and the two
  D6-only ones), `ignoremark-no-target` / `ignoremark-parse`, and `all-ends:*`, the entry points
  that ask for every end at a position and have no compiled goal (`match-all` behind `:ov`/`:ex`,
  `ltm-lookahead-fate`, `token-method`, `grammar-probe`).
- **`bridged`**: a compiled run handed a piece back to the walk. A `<subrule>` call's reason is the
  first check of `rx_call_target` it failed (`args`, `lexical-regex`, `dynamic-param`, `wrapped`,
  `custom-how`, `rule-dynvar-decls`, `left-recursion-active`, `no-candidates`, `ignoremark`,
  `multi-candidate`, `proto-inherited-i`, `qq-thunks`, `left-reenter`, `callee-declined`,
  `grammar-method`); besides calls, `quantified-call` (`<x>*` through the single-candidate arm),
  `ratchet-scan` (the possessive `NamedRun` scan) and `code-interp` (an interpolated pattern's ends).
- **`leaf`**: one atom of a compiled program that the walk's single-atom arm matched
  (`builtin-call`, `lookaround`, `backref`, `marker`, `closure-interp`, `ws-rule`, `var-interp`,
  `qq-interp`). These do not walk a tree, but they live in the walk's modules and must move out
  before those are deleted.

`scripts/rx-decline-survey.sh` sums the line across files as it sums `regex-vm:`.

### Slice E, second part: every end at a position is compiled

`Grammar.parse`'s goal (`UntilFull`) becomes `Goal::Ends { out, stop_at_full }`: at the pattern's
own `Match` the run records the end and backtracks for the next one, stopping at the first end that
covers the subject only when asked. `regex_match_ends_from_caps_in_pkg`, the walk's "every end at
`start`" entry, now tries it first (`rx_try_all_ends`), so `:ov`/`:ex`, LTM lookahead fates, cursor
token methods and the walk's own sub-pattern calls (an alternation branch of a declined pattern)
run compiled whenever their pattern has a program. A `:m` pattern keeps the walk there
(`ignoremark-ends`): the walk remaps the whole end list across the stripped subject.

The differential sweep found one bug that the change exposed: the parse-failure probe
(`all_complete_match_ends_max`) built an unanchored copy of the start pattern with
`..(*parsed).clone()`, which shared the original's `derived` analyses, so the copy ran the
original's compiled program, `$` included. A pattern built by struct update from another now gets
fresh `derived` analyses there, as `regex_match_atom`'s scoped `:i` copy already did.

### Slice E, third part: frugal quantifiers under ratchet

`frugal-ratchet`, the most common decline left in `t/grammar` and `t/regex`, compiles. In rakudo a
frugal quantifier keeps growing on demand under ratchet; only each iteration's atom (and separator)
commits. So the loop keeps its choice point (no `whole` cut when frugal) and the per-iteration cut
stays, for `*?`, `+?`, `**?`, `??` and `% sep`. Comparing with rakudo found two walk bugs, fixed in the
walk the same way: a ratcheted `??` tried the atom before the empty arm, and a ratcheted `+? % sep`
stopped at its minimal count (`match_separated_quantifier_ratchet` now offers every length, the
shortest preferred). A separated one whose atom or separator runs code still declines
(`separator-frugal-ratchet-code`): the walk grows that chain eagerly, so the code would run a
different number of times.

### Slice E, fourth part: nullable loop bodies, and quantified backreferences

`nullable-loop` compiles. A non-ratcheted loop whose body can match empty and has more than one
candidate in principle, but which the walk's chain grows one first candidate per iteration
(`grow_one_iter`: a backreference, an interpolation), is committed per iteration like the
conjunction body before it, so a `ZeroIter` rejection stops the loop instead of retrying the body.
Rakudo has nothing to compare with on the purely empty cases (`"aab" ~~ /[a?]* b/` loops forever
there), so the walk's chain is the reference, as D6 checks.

The shapes that reach this are almost all quantified backreferences, and none of them had ever
matched: the regex parser pushed `$0` / `$<name>` and moved on, so the quantifier after it was never
read (`"aab" ~~ /(a) $0+ b/` was `Nil`; rakudo `｢aab｣`). A backreference now goes through the common
atom path, which reads its quantifier.

### Slice E, fifth part: `:m` on the compiled engine

A scoped `[:m …]` group compiles to `GroupEnds`: the walk matches such a group by asking for every
end of the body over the mark-stripped subject and mapping them back (`ignoremark_on_target`), so
the op asks the same all-ends entry and enters the ends highest priority first, cut under ratchet,
as `InterpEnds` does. That entry, and a whole-pattern `:m` asked for every end (`:ex`), now run the
stripped pattern's program there (`rx_try_ignoremark_ends`) instead of walking. A group whose body
runs code or holds a backreference still declines (`ignoremark-code`): those read the enclosing
level through the walk's inline seeds, which the nested run does not arm.

### Slice E, sixth part: calls with arguments run as frames

A `<name(…)>` call no longer bridges for having arguments. The Call op evaluates them once, against
the caller's captures, and resolves the callee for those values (`rx_call_target_args`, over the
memo `parsed_subrule_candidates` keeps per rendered argument list); a plain rule or a proto whose
candidates all have programs then runs as a frame, as an argument-less call does. Every other
verdict still bridges, but hands the evaluated values to the producer
(`regex_match_atom_all_with_arg_values`), so user code in an argument never runs twice. The
blockers that apply to every call (`$*` parameters, wrapped tokens, custom HOWs, a live
left-recursion activation) are checked before the arguments are evaluated.

One more shape bridges, with its evaluated arguments: an object or closure argument
(`args-opaque`). Baking cannot carry such a value into the callee's code blocks, so the walk binds it
in the env for the callee's match window (`install_subrule_dynamic_params`); a frame the run can
backtrack into would need that binding re-installed and removed as backtracking crosses the frame,
the same enter/exit pair `isolated-group-scoped` needs.

### Slice E, seventh part: closure scopes as an undoable op pair

`isolated-group-scoped` compiles. A spliced Regex value that closed over its own scope is now an
isolated group between `ScopeEnter` and `ScopeExit` (`rx_scope`): the first installs the scope in the
env (`install_env_scope`), the second uninstalls it, and each also pushes an entry on the register
trail under a tag no register index reaches. Rewinding past a `ScopeEnter` uninstalls the scope;
rewinding past a `ScopeExit` installs it again. The trail is untouched by a ratchet's cut, so a
committed body still unwinds; whatever a run leaves installed when it ends (a failure inside a body
whose trail entries a settled return or an empty stack had dropped) is uninstalled at the end of the
run. The body is therefore matched lazily, as the walk matches it, and D6 agrees with the walk's code
invocation order, which an eager all-ends op (tried first) did not.

The same pair is what `args-opaque` and the `$*`-parameter blocker need: a binding installed for a
callee's match window, kept correct as backtracking crosses the frame.

### Slice E, eighth part: call frames with a binding window

`args-opaque` and `dynamic-param` compile. A `<subrule>` call whose callee needs a binding window
(its `$*` parameters, or an object or closure argument that baking cannot carry into its code
blocks) runs as a frame, like any other plain or proto call. The window is the one the walk's
producer installs around the callee's whole match (`install_subrule_dynamic_params`). The Call op
installs it before it resolves the callee (`rx_call_resolve`), because the callee's pattern may
interpolate a `$*` parameter. It is recorded in the run's scopes (`rx_scope`, generalized from the
closure scopes of the seventh part) and trailed under the same tags:

- the install pushes an undo-install entry, so a failure that rewinds past the call removes it;
- the callee's return uninstalls it and pushes an undo-uninstall entry, so backtracking into the
  callee installs it again. What the window held at the uninstall is what goes back, so a write the
  callee's code made to a `$*` parameter survives the round trip;
- a proto's candidates all run in the one window: it is installed before the proto's choice point is
  pushed, so moving to the next candidate keeps it;
- the return also records the window's values on the callee's Match, as the walk does
  (`attach_grammar_dynvars_to_named_caps`), because the callee's action runs later, in the reduce walk.

The `ANY_DYNAMIC_TOKEN_PARAM` blocker is gone with it: once any rule in the program had a `$*`
parameter, every call of every rule bridged. A call that bridges for another reason installs
nothing; the producer installs its own window as before.

D6 cannot compare a non-ratchet (`regex`) callee with arguments and code: the walk computes such a
callee's ends eagerly, so it runs the callee's code at every end before the caller continues, where
a frame runs it lazily (rakudo's order). That disagreement predates this part (the sixth part made
calls with arguments frames) and is not a correctness issue; it goes with the walk.

`t/grammar/grammar-subrule-binding-window-frames.t` pins the rakudo values (re-binding on
backtracking into a callee, nested windows, a failed call, proto candidates, actions). Found on the
way, in both engines: a proto's own `$*` parameter (`proto token p($*K) {*}`) is never bound
([#11071](https://github.com/tokuhirom/mutsu/issues/11071)).

### Slice E, ninth part: grammars that declare `:my $*x`

`context:rule-dynvar-decls` and the `rule-dynvar-decls` bridge are gone. Before this part, a match ran
on the walk whenever the grammar being parsed had any rule that declared a dynamic variable
(`token r { :my $*x = …; … }`), and so did every call made in it. The walk initializes such
declarations at rule entry (`enter_grammar_rule_dynvars`), restores them when the invocation ends,
and marks the keys as owned by a live rule frame. The rule's `VarDecl` atom then records the value
instead of running the initializer again.

A frame call now does the same. Once the call is known to be a frame, `rx_call_rule_frame` enters the
callee's rule frame. A bridged call enters none, because its producer enters its own, so an
initializer never runs twice. The frame joins the call's binding window of the eighth part
(`CallWindow`): its shadowed bindings follow the `$*` parameters, and its keys are recorded on the
callee's Match for the action. The window also carries the frame's "owned" mark, which `rx_scope`
pushes and pops with every install and uninstall (`grammar_dynvar_scope_push`). Backtracking into the
callee therefore finds its declaration live and marked, as at the first entry.

The comparison with rakudo found the walk wrong in such grammars, and the compiled engine right. With
declarations present, the walk does not commit a ratcheted subrule to its first end (the
`ratchet && grammar_rule_dynvar_decls.is_empty()` argument of its `for_each_atom_candidate` call), so
a `token` callee was re-entered for its shorter ends. Rakudo commits. So D6 reports a code-order
disagreement on such grammars (`t/grammar/ipv6-mapped-dotted-decimal.t`); the results agree.

Survey over `t/grammar`, `t/regex` and `t/modules`: walk uses fell from 11,469 to 10,538.

- `walked` 4,537 → 3,352. `context:rule-dynvar-decls` 1,273 → 0.
- `bridged` 4,909 → 4,985. The matches that ran on the compiled engine for the first time now reach
  their declined callees through the bridge instead of walking whole (`callee-declined` +72).

`t/grammar/grammar-rule-dynvar-decl-frames.t` pins the rakudo values: ratchet commit, scope and
shadowing, re-installation on backtracking, and the action's view.

### Slice E, tenth part: quantified calls are loops of frame calls

`ratchet-scan` and `quantified-call` are gone. A quantified `<subrule>` (`<x>*`, `<x>+`,
`<x> ** n`, `<x>+ % sep`) compiled to a loop whose body asked the walk's single-candidate arm for
the callee's first end (`CapAtom`). A ratcheted `*` / `+` was first offered to the walk's
possessive scan (`NamedRun` over `regex_named_ratchet_run`). The loop body is now the same `Call`
op as an unquantified call, committed under ratchet, and the `NamedRun` op is deleted.

That also fixes a wrong answer, with rakudo as the reference. The walk's chain took each
iteration's first end only, so a later failure could never backtrack into an iteration's callee:
`regex TOP { <x>+ a }; regex x { a+ }` failed on `aaa`, where rakudo matches with `x => aa`. An
uncommitted frame resumes there (Slice D's non-ratchet resumption). D6 therefore disagrees with the
walk on that shape.

The walk's scan dropped the `:sym<…>` of a proto with a single candidate on each iteration's Match,
and D6 caught it (`t/grammar/role-proto-regex-reinstantiate.t`). The scan's resolution
(`try_resolve_named_to_pattern`) now returns the candidate's sym, and the scan files it.

Measured on release builds, best of five, against `main`: `bench-grammar-parse-big`,
`bench-grammar-json-tiny`, `bench-yaml-parse` and `bench-yaml-parse-big` are unchanged within noise.
So removing the scan's fast path costs nothing measurable.

Survey over `t/grammar`, `t/regex` and `t/modules`: walk uses fell from 10,563 to 9,098.

- `bridged` 5,010 → 3,608: `ratchet-scan` 1,239 → 0 and `quantified-call` 211 → 0.
- `walked` 3,352 → 3,281.

`t/grammar/grammar-quantified-subrule-frames.t` pins the rakudo values.

### Slice E, eleventh part: `<&lexical>` calls, and wraps narrowed to the wrapped rule

Two more bridges become frames. Both were shapes #7548 lists as still eager.

- **`<&r>` naming a lexical Regex** (`lexical-regex`). Such a call resolves per call: a lexical's value
  belongs to the call's scope, so the verdict is never cached. The Regex's defining scope joins the
  call's binding window (`install_subrule_dynamic_params`, which the walk's producer installs too).
  `<::(EXPR)>` still bridges, under its own reason, `symbolic-name`.
- **Wrapped tokens** (`wrapped`). The blocker used to be global: one `.wrap` on any method sent every
  later call of every grammar to the walk. The reason was that a wrapper reads its caller's rule name
  from a Backtrace (#9151), out of the routine frame the walk's eager arm pushes around each call while
  a wrap exists (`subrule_candidate_ends_with_frame`). That routine frame is now part of the call
  window (`CallWindow::routine`), pushed and popped like the bindings: installed while the callee
  runs, removed at its return, and pushed again when backtracking re-enters it. Only a call of the
  wrapped rule itself bridges, since its wrapper is user code around the invocation.

What #7548 measured as eager is now lazy for proto candidates, calls with arguments, `$*` parameters,
lexical Regex calls and unwrapped rules in a wrapped grammar. Each `{ … }` block in the callee runs
once per end entered, as rakudo runs it (`t/grammar/grammar-lexical-and-wrap-subrule-frames.t`). Still
eager:

- a call of a wrapped rule;
- custom-HOW grammars and grammar methods;
- several candidates without a proto;
- left recursion.

Rakudo gives no reference for the last two: it reports two such `multi regex` candidates as an
ambiguous call, and it loops forever on left recursion. A wrapped proto candidate is not honored by
either engine ([#11151](https://github.com/tokuhirom/mutsu/issues/11151)).

### Slice E, twelfth part: `<~~>`

`recurse-self` is no longer a decline. In the walk, `<~~>` takes the enclosing regex's first end at the
cursor and discards its captures. It is guarded against re-entering at the same position
(`regex_match_recurse_self`). That leaf matches the regex through
`regex_match_end_from_caps_in_pkg`, which answers from the regex's compiled program, so the atom
compiles to a `CapAtom` leaf (`leaf=recurse-self`), the way a lookaround does, and no walk is entered.
The 7 declined patterns over `t/` and the roast whitelist compile
(`t/regex/regex-recurse-self-compiled.t`, rakudo's values).

The most common whole-pattern decline left is `seqalt-nullable-ratchet`. Rakudo's rule for it turned
out to depend on sigspace after the group, and the walk's heuristic does not follow it
([#11162](https://github.com/tokuhirom/mutsu/issues/11162)).
### Slice E, thirteenth part: left recursion without the bridge

`left-reenter` and `left-recursion-active` are gone. The growing-seed loop that evaluates a
left-recursive call moved out of the walk's eager `Named` arm into its own module, `regex_lr_seed.rs`
(`subrule_seed_ends`). It takes the candidate-evaluation helpers with it, and `regex_match_atom.rs`
drops from 1,216 to 844 lines. The walk's arm calls the module, and so does the compiled engine,
which now resolves such a call to `CallTarget::Lr` (renamed `Eager` in the fourteenth part). A call becomes `Lr` in two cases:

- the call graph cannot prove the rule never re-enters itself at the same position (the old
  `left-reenter` verdict);
- an evaluation of the same name is live further up (the old `left-recursion-active` blocker). Such
  a call may be the re-entry, which reads the live seed.

The loop is the algorithm D3 required to stay confined to those calls. Its candidates are evaluated
through the all-ends entry, which runs their compiled programs. So the walk takes no part, and the
call counts as a leaf (`leaf=lr-seed`). The evaluation is eager, as before. The call's binding window
(`$*` parameters, the rule's `:my $*x`) is installed around it only, and its final values are filed
on each end's Match for the action, as the walk's producer filed them.

Nothing about the answers changes: the loop is the walk's own, moved. D6 found no disagreement it did
not find on `main`. Rakudo has no reference here, since it loops forever on left recursion.
`t/grammar/grammar-left-recursion-compiled-call.t` pins mutsu's values.

Survey (`t/grammar`, `t/regex`, `t/modules`): `bridged` 3,501 → 3,178. `left-recursion-active` 369 → 0
and `left-reenter` 200 → 0; the calls run as `leaf=lr-seed` (321). `callee-declined` rose by 246:
calls that used to stop at the old blocker now reach the declined `:m` callee check.

### Slice E, fourteenth part: `(:m …)` captures, and eager calls of declined callees

Two changes, and almost all of the remaining walk uses go with them.

- **A capture group with a scoped `:m` body** (`(:ignoremark '"')`) used to decline its whole
  pattern. It now compiles like `[:m …]`, with a `GroupEnds` over the body's ends on the
  mark-stripped subject, inside the capture's own level. JSON::Tiny's string token and
  `t/regex/regex-ignoremark-scaling.t` were the main users, and they now run with no walk at all.
- **A call whose callee has no program** (`callee-declined`, and the `ignoremark` verdict for a `:m`
  callee) is no longer a bridge. The left-recursion verdict of the thirteenth part is generalized to
  `CallTarget::Eager(candidates, reason)`, and these calls take it. The growing-seed loop evaluates
  them through the all-ends entry, which runs a `:m` callee's stripped program, so the call is a leaf
  (`declined-callee`, `ignoremark-callee`). A callee that truly declines is still walked. The
  all-ends entry counts that use itself, as `walked=declined`, so nothing is hidden.

Survey (`t/grammar`, `t/regex`, `t/modules`):

- `walked` 3,164 → 488;
- `bridged` 3,183 → 138;
- uses of the walk, walked plus bridged: 6,347 → 626.

What still bridges: `code-interp` 68, `grammar-method` 23, `custom-how` 15, `wrapped` 15,
`args-method` 10, `qq-thunks` 5, `no-candidates` 2.

What still walks a whole match: `declined` 410, from the 31 patterns still declined. The most
common decline is `seqalt-nullable-ratchet` ([#11162](https://github.com/tokuhirom/mutsu/issues/11162)).
Context declines add 78 more.

### Slice E, fifteenth part: a ratcheted `||` commits; sigspace whitespace un-ratchets the term before it ([#11162](https://github.com/tokuhirom/mutsu/issues/11162))

`seqalt-nullable-ratchet` is gone. The walk treated a zero-width first branch of a ratcheted `||` as
provisional: it moved on to the next branch while the rest of the pattern had not matched. The
compiled engine could not express that, so it declined such patterns. Rakudo has no such rule.
Its `altseq` commits to the first branch that matches, zero-width or not.

What the heuristic stood in for is where rakudo puts the ratchet. RakuAST (`src/Raku/ast/regex.rakumod`,
rakudo 2026.09) ratchets each term of a sequence on the term's outermost compiled node. A term
that sigspace whitespace follows is a `WithWhitespace`, which compiles to `concat(term, <.ws>)`.
The ratchet lands on that concat, which ignores it, so the term itself stays backtrackable. This
holds for every term, not only `[ … || … ]`. A subrule call, a quantifier, a `|`, a `||` and a
capture group followed by significant whitespace can all give back their match. The same term
without whitespace after it commits. Three details follow from the same code:

- a sigil alias (`$<x>=…`) ratchets the atom it binds itself (`NamedCapture` applies the ratchet
  to its target), so `$<x>=<b> '!'` commits where `<x=b> '!'` does not;
- a `[ … ]` group compiles to its body, so a backtrack decision made for `[<b>]` is made for `<b>`;
- an explicit `<.ws>` is an ordinary ratcheted term and changes nothing.

Before this part, mutsu followed the rule for quantifiers only (`<id>? <v>`, #10569). It now
applies it to every term that can backtrack, in both places sigspace is lowered: the `rule` text
pass (`rule_ws_quantified::mark_backtracking_before_ws`, which writes `:!` after the term) and
the `:s` parser (`sigspace_term_backtracks`). With that, a ratcheted `||` commits in both engines,
and the walk's heuristic and the decline are deleted. `t/grammar/grammar-sigspace-term-ratchet.t`
pins rakudo's values. `t/grammar/grammar-optional-ordered-alternative.t` (a `rule` with whitespace
after `]`) passes unchanged.

Rakudo's legacy QRegex frontend lowers sigspace the same way (`quantified_atom` wraps the atom in
`concat(atom, sigfinal)` before it applies the ratchet). One rakudo behaviour is not followed. A
non-capturing `<.b>` followed by whitespace sometimes commits in rakudo (`rule { <.b> '!' }` with
`regex b { <[x!]>+ }`). The same call to a callee with a different body backtracks
(`regex b { <[x!]> <[x!]>? }`). No rule in the source explains the difference, so mutsu treats
`<.b>` like `<b>`.

Un-ratcheting the calls exposed a bug in the grammar-method bridge. When a user
`method ws { nextsame }` deferred to a built-in that failed, it returned a cursor with a negative
`pos`. The bridge read that cursor as a zero-width success.

Survey (`t/grammar`, `t/regex`, `t/modules`):

- declined patterns: 31 → 10. What remains is `repeat-code` 4, `separator-alias` 3,
  `repeat-code-nullable` 1, `goal-match-code` 1 and `conjunction-backref` 1;
- `walked` 488 → 151, of which `declined` is 410 → 81;
- `bridged` 138 → 181. The newly compiled patterns now reach their calls' bridges instead of walking
  whole: `grammar-method` 23 → 56, `wrapped` 15 → 22 and `args-method` 10 → 13.

Release builds, best of five, against `main`: `bench-grammar-parse{,-big,-deep}`,
`bench-grammar-json-tiny` and `bench-yaml-parse{,-big}` are unchanged within noise. These grammars
use `rule`s, so their calls are now uncommitted wherever whitespace follows them.

### Slice E, sixteenth part: the last whole-pattern declines

Five decline reasons go:

- **`separator-alias`.** A name on a separated quantifier's own token (`<alpha>+ % ','`,
  `<x=alpha>+ % ','`) is applied over each atom's span with a `Named` op. The walk's chain applies
  it per iteration in the same way. The iterations file it in place or in their own levels, like
  any other name under the quantifier.
- **`repeat-code` with a separator.** `atom ** { code } % sep` evaluates its count where the
  quantifier is reached (`RepeatCount`), as `x ** { code }` does. The bounds are then checked
  against registers: the loop head runs as `RepeatDyn` over a zero minimum, the first atom (outside
  the loop) is checked against the maximum (`AtMostReg`), and the exits against the minimum
  (`AtLeastReg`). The walk's separated quantifier now evaluates the count through
  `regex_repeat_count`, the recorded call `RepeatCount` uses, so D6 compares the two.
- **`repeat-code-nullable`.** A body that can match empty under `** { code }` gets a `ZeroIterDyn`
  guard, which reads the bounds from the `RepeatCount` registers.
- **`conjunction-backref`.** A backreference in a `&` branch reads the enclosing level, as code
  there does. The first branch opens an inline level, and the other branches' nested runs are
  seeded with the same view.
- **`goal-match-code`.** Both sides of `~` belong to the enclosing regex, in the walk (the
  outer-captures seed) and in rakudo (one cursor). A side that reads enclosing state or holds a
  backreference gets an inline level instead of an isolated one. A side matched in place sees the
  enclosing captures as they are.

Survey (`t/grammar`, `t/regex`, `t/modules`): the only decline left is `seqalt-nullable-ratchet`,
which the fifteenth part removes. With both parts, `declined` is 0 over these directories.
`t/regex/regex-last-declines-compiled.t` pins rakudo's values. The patterns run with no walk
under `MUTSU_RX_DIFF=1` too.

### Slice E, seventeenth part: an interpolated pattern runs as a lazy frame

The `code-interp` bridge goes. `$( … )` / `@( … )` is the last call-out whose ends were computed
up front: `InterpEnds` ran the code, parsed the pattern it yielded, asked the all-ends entry for
every end and entered them through one `Choice::Cands`. Code inside the yielded regex therefore ran
at every end, where rakudo's interpolated regex is a cursor resumed on demand:
`my $r = rx/ a+ { $n++ } /; "aaab" ~~ / $($r) b /` ran the block three times instead of once.

`InterpEnds` now runs the code once, where the cursor reaches it (`regex_code_interp_parsed`). If
the yielded pattern has a program, the op enters it as a frame. An *interp* frame
(`Frame::interp`, whose `site` is the `CodeInterp` atom) is entered and resumed like a call's, so
backtracking into it continues from its own choice points. Its return drops the callee's level
instead of filing a subrule Match: rakudo keeps none of an interpolated regex's captures
(`"ab" ~~ / @( rx{ (\w) } ) b /` has no `$0`). Both engines kept them before; both drop them now.
A yielded pattern that declines is still matched up front, counted as `leaf=code-interp-declined`.
The program that holds the op runs the frame-capable loop (`has_call`).

D6 records the code's run on its own (`CodeResult::Source`, the pattern source it yielded), so the
two engines are compared on the source they get. A yielded regex that runs code is the one shape D6
cannot compare: the walk still matches it up front, and so runs that code at every end.

`t/regex/regex-code-interp-lazy-frame.t` pins rakudo's values (code run counts, backtracking,
dropped captures, ratchet). Found on the way: a `/`-delimited `rx/…/` inside `$( … )` in a `/…/`
regex ends the outer literal early ([#11606](https://github.com/tokuhirom/mutsu/issues/11606)).

### Slice E, eighteenth part: a grammar method is a leaf

The `grammar-method` bridge goes, and so does `args-method` where the call names a method. A
`<name>` call with no rule of that name that names a plain grammar METHOD (`<.panic>`,
`<.expect('x')>`) now resolves to `CallTarget::Method`. The engine calls the method once, on
the calling frame's cursor (`rx_cursor_of`, #9803), and takes the one end it answers, as it does
for a builtin (`Single`). No choice point is pushed. A call that backtracks to the same site
calls the method again at the new position, as rakudo does.

The method call moved out of the walk's producer into `regex_grammar_method.rs`
(`regex_grammar_method_end`). The walk's `try_regex_subrule_as_method` is now a thin entry over
it. The method is user code, so D6 records and replays it like a code atom (`rx_code_call`). In
`MUTSU_RX_DIFF` runs, both engines used to call it, so its side effects happened twice. The
pending-exception check is part of the recorded call, so a method that dies is replayed as well.

What is left of `args-method` is renamed `args-unbound`: a call with arguments that no
candidate binds (a parameter type error the producer raises) or that names a builtin. Survey
(`t/grammar`, `t/regex`, `t/modules`, debug build): `grammar-method` 56 → 0 and `args-method`
13 → `args-unbound` 10. The bridges left are `wrapped` 22, `custom-how` 15, `args-unbound` 10,
`qq-thunks` 5 and `no-candidates` 2. `t/grammar/regex-grammar-method-leaf.t` pins rakudo's
values (zero-width `self`, arguments, a failed cursor, a returned Match's extent, the exception,
call counts under backtracking, the cursor the method writes). The same file passes under
`MUTSU_RX_DIFF=1`. Found on the way: a user type named `Cursor` resolves to `Match`
([#11705](https://github.com/tokuhirom/mutsu/issues/11705)).

### Slice E, twentieth part: the leaf atoms leave the walk, and no call bridges

**Leaf atoms.** These atoms answer at most one end:

- a builtin call (`<alpha>`, `<ws>`, `<wb>`, `<:Letter>`, an unknown name)
- a backreference
- `<(` / `)>`
- `$x`
- `"…"`
- `<{ … }>`
- `<~~>`

The compiled engine matched them through the walk's single-candidate matcher. Their definitions
now live in `regex_atom_leaf.rs`, outside the walk's modules, so the walk can be deleted without
them (D7). `regex_leaf_atom` matches one of these atoms and `regex_builtin_named` matches a call
of no rule. The compiled engine's `CapAtom` op and its `Single` call target call them directly.
The walk's arms for the same atoms call them too, so each atom has one definition (D4). `CapAtom`
still reaches the walk for one atom only: a lookaround, which the nineteenth part compiles.

**Call bridges.** Every remaining call bridge goes:

- **`args-unbound` and `no-candidates`** become `Single`. A call whose arguments no candidate
  binds, or whose candidates none parses, is matched as a call of no rule, as in the walk. The
  type error was raised when the arguments were resolved.
- **`qq-thunks`** become frames. The callee's `"…"` atoms read their qq thunks' results, which
  the thunks produce at rule entry. Those results join the call's binding window
  (`install_subrule_qq_thunks` in `rx_call_window`), so backtracking removes and re-installs them
  with the frame. The callee is now resumed lazily: in
  `regex t { "$x" a+ { $n++ } }` called from `regex TOP { <t> 'ab' }`, the block runs twice on
  `qaaab`, as in rakudo. The bridge computed every end up front and ran it three times.
- **`wrapped`** becomes `CallTarget::Wrapped`. The wrapper is user code around the rule's
  invocation and answers at most one end (`try_wrapped_token_subrule_dispatch`). A proto with a
  wrapped candidate is evaluated eagerly, as `Eager(…, "wrapped-candidate")`, because the
  growing-seed loop dispatches the candidate's wrapper.
- **`custom-how`** becomes `CallTarget::CustomHow`. The custom HOW's `find_method` may hand back
  a wrapper to run (`try_custom_how_subrule_dispatch`). Otherwise the candidates go through the
  growing-seed loop with its left-recursion bookkeeping forced, as the walk's producer does. The
  old blocker bridged every call of every grammar once any custom HOW existed. Now only a call
  of a rule takes this path.

All three eager shapes (`Eager`, `Wrapped`, `CustomHow`) share `rx_eager_call_ends`. It holds the
call's window around the evaluation only.

Survey (`t/grammar`, `t/regex`, `t/modules`, debug build):

- `bridged` 54 → 0 (`wrapped` 22, `custom-how` 15, `args-unbound` 10, `qq-thunks` 5,
  `no-candidates` 2).
- Leaves 1872 → 0: `builtin-call` 1408, `marker` 144, `var-interp` 127, `closure-interp` 71,
  `backref` 65, `recurse-self` 43 and `qq-interp` 14.
- Two eager leaves are new: `custom-how` 8 and `wrapped-candidate` 2.

`t/regex/regex-leaf-atoms-compiled.t` and `t/grammar/grammar-call-bridges-compiled.t` pin
rakudo's values. Under `MUTSU_RX_DIFF=1`, the same 16 files fail here and on `main`, each with
the same first disagreement. The new grammar test also disagrees there: the walk still evaluates
the qq-thunk callee eagerly, so its replay runs the block a third time.

### Reproducing §2

```raku
# §2.3 workload (the prototype runs the same patterns over the same subject)
my $unit = "the quick brown fox 12345 jumps over 67-8 lazy\n";
my $big  = $unit x (655360 div $unit.chars);
for ^3 { my $s = now; my $r = so $big ~~ / \w+ \s \d ** 6 /; say ((now - $s) * 1000).round }
for ^3 { my $s = now; my $r = so $big ~~ / [ \w+ \s ] ** 3 \d ** 6 /; say ((now - $s) * 1000).round }
```

The §2.1 grammar row loops `JsonLike.parse($doc)` 30 times over `bench-grammar-parse-big.raku`'s
own grammar and document (`$PAIRS = 320`) and reports the median of iterations 26–30.
