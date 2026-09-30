# ADR-0135: A regex compiles to a flat backtracking program; the tree walk is retired

- **Status**: Accepted (2026-09-30; proposed and accepted the same day); Slice A in progress (§8). Slices tracked as
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
  dynamic rule parameters, custom-HOW grammars. When the D5 counter reads zero: delete the walk, the
  eager `Named` arm and the D6 mode, and close #7548.

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
| [#10225](https://github.com/tokuhirom/mutsu/issues/10225) fate of `:P5` | independent | `:P5` runs on pcre2, not on the walk. It is outside D7's "one engine", and either decision leaves this ADR unchanged |
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

Still to come in Slice A: backreferences, `%` separators, nested quantified captures, nullable
loop bodies, `<( )>` markers, `CompositeClass`, and moving the unanchored scan loop into the VM.

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
