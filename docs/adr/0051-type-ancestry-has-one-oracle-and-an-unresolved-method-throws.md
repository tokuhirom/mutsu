# ADR-0051: Type ancestry has one oracle, and an unresolved method throws instead of stringifying

- Status: Proposed (design complete; implementation not started)
- Date: 2026-08-20
- Supersedes: none
- Related: [ADR-0019](0019-compiled-declarations-and-unified-method-dispatch.md) (§2 "One registry owns
  every type×method entry"; Phase E resolver, Phase F2 introspection derivation), and
  ADR-0047 *(not yet merged — open PR #6745; registry key identity, a disjoint concern: that ADR
  decides **which** registry entry a name denotes, this one decides **what the entry says about
  ancestry** and **what happens when nothing matches**)*.

Picks up `todo/deep/plain-classes-answer-cool-only-builtin-methods.md`, re-measured end to end
against `main` on 2026-08-20. **Design only — no behaviour change in the PR that lands this ADR.**

## Context

### The reported symptom

```raku
class G {}
say G.new.uc;   # raku: dies "No such method 'uc' for invocant of type 'G'"
                # mutsu: "G()"
```

A plain class derives from `Any`/`Mu`, not `Cool`, so `.uc` is not in `G`'s MRO. mutsu answers
anyway. Re-verified on `main` — the symptom is live, not stale.

Sweeping the `Cool` method surface on a plain class (`tmp/cool_sweep.p6`, 60 names, `raku` vs
`target/debug/mutsu`) gives **26 divergences**, and the shape of the set is the first useful fact:

| Diverging (raku dies, mutsu answers) | Agreeing (both die) |
|---|---|
| `uc` `lc` `tc` `tclc` `fc` `flip` `chars` `codes` `ords` `chop` `trim` `trim-leading` `trim-trailing` `words` `lines` `encode` `NFC` `NFD` `NFKC` `NFKD` `unival` `univals` `IO` `chr` `ord` `sprintf` | `abs` `sign` `sqrt` `ceiling` `floor` `round` `truncate` `exp` `log` `log10` `sin` `cos` `tan` `succ` `pred` `base` `Date` `DateTime` `Instant` … |

Every divergence is a method whose implementation **stringifies the receiver**; every *numeric*
`Cool` method already behaves correctly. So the leak is not a generic "Cool catch-all" — it is
specifically the **string-coercion** family, and it is structural rather than name-driven.

Two further probes sharpen it:

```raku
class S { method Str { "hello" } }
say S.new.Str;   # mutsu "hello"  (correct)
say S.new.uc;    # mutsu "S()"    — raku DIES; and note mutsu does not even use S's own .Str
class G {}
say G.^can("uc").elems;   # mutsu 0  — matches raku's 0
say G.new.uc;             # mutsu answers anyway
```

So `.^can` **already gives the right answer** for a plain user class, and dispatch contradicts it.
This is the crux: mutsu does not have one wrong answer, it has several mechanisms that disagree.

### The previous investigation's conclusion, and why it was wrong

The `todo/deep` file records a tried-and-reverted fix: gate the three existing call sites on
`!class_mro_includes_cool(&class_name)`. That made `G.new.uc` die correctly and flipped
`t/handles-wildcard-builtin-methods.t`'s two `todo`-marked assertions to passing — but broke six
test files. It concluded that those six were *"coincidentally-named genuine per-type native
methods"* having *"NOTHING to do with `Cool` inheritance"*, and that landing any general fix
therefore needs a new cross-module `(class, method)` row-existence predicate which *"does not exist
yet"* because `RAW_ROWS` is `pub(super)`.

**Both halves of that conclusion are falsified by measurement.**

**(a) The predicate already exists.** `native_method_row_exists(owner, name)`
(`src/builtins/native_method_row.rs:213`) is `pub(crate)`, and
`Interpreter::e2_native_method_exists(&Value, &'static str)`
(`src/runtime/receiver_class.rs:490`) already wraps it with the full dispatch-chain walk plus the
`canonical_builtin_owner` fold. Both were added for ADR-0019 Phase E box E7 step 4 (`.^can`) and
are consumed from `runtime/` today. The todo file predates them.

**(b) Three of the six regressions were not "coincidental names" at all — they are types that
genuinely *are* `Cool` in Rakudo, whose ancestry data in mutsu is simply wrong.** The todo file
generalized from a single `DateTime` measurement (`DateTime.^mro` really is `(DateTime, Any, Mu)`)
to all six. Checking each against `raku`:

| Type | Rakudo `.^mro` | Cool? | mutsu `.^mro` | mutsu `~~ Cool` |
|---|---|---|---|---|
| `DateTime` | `DateTime, Any, Mu` | no | `DateTime, Any, Mu` | no |
| `Date` | `Date, Any, Mu` | no | `Date, Any, Mu` | no |
| **`Instant`** | `Instant, **Cool**, Any, Mu` | **YES** | `Instant, Any, Mu` ✗ | **YES** ✗ |
| **`Duration`** | `Duration, **Cool**, Any, Mu` | **YES** | `Duration, Any, Mu` ✗ | **YES** ✗ |
| **`IO::Path`** | `IO::Path, **Cool**, Any, Mu` | **YES** | `IO::Path, Any, Mu` ✗ | no ✗ |
| **`Match`** | `Match, Capture, **Cool**, Any, Mu` | **YES** | `Match, Any, Mu` ✗ | **YES** ✗ |

A 27-type sweep (`tmp/mro_sweep.p6`) finds **exactly these four** types disagreeing with Rakudo;
all others (`Str`, `Int`, `Num`, `Rat`, `Bool`, `Array`, `Hash`, `List`, `Seq`, `Range`, `Complex`,
`Pair`, `Set`, `Bag`, `Mix`, `Version`, `IO::Handle`, `Any`, `Mu`, `Cool`, …) agree. And three of
the four are **internally inconsistent**: `~~ Cool` says YES while `.^mro` says no, in the same
process, about the same type.

So the six regressions decompose as:

| Regression | Real cause | Correct fix |
|---|---|---|
| `Instant.abs` | `Instant` **is** `Cool`; mutsu's ancestry data omits it | fix the ancestry data |
| `IO::Path.chars` | `IO::Path` **is** `Cool`; ancestry data omits it | fix the ancestry data |
| `Instant.DateTime` | genuine own method (`Instant.^can("DateTime")` is 1 in raku) | add the row |
| `DateTime.Date` | genuine own method — **already has** row `("DateTime","Date",1,0)` | nothing |
| `Date.IO` | genuine own method in raku (`Date.^can("IO")` is 1); mutsu's `^can` is **0** — today's answer is the stringify catch-all landing on the right value **by luck** | add the row |

`("Cool","abs")` and `("Cool","chars")` already exist in `RAW_ROWS` (lines 1171, 1143), so fixing
`Instant`'s and `IO::Path`'s ancestry makes `e2_native_method_exists` answer them through the
`Cool` block with no new rows. Only two genuine rows are missing.

**The measurement therefore inverts the todo file's plan.** It is not "build a predicate, then
restructure three gates". It is "**fix the ancestry data so the oracles agree, then gate**" — and
the gate can use machinery that already shipped.

### Why the data is wrong: ancestry is answered by twelve tables

`Cool`-ness has no single owner. Each of these is consulted by a different consumer, and they are
independently maintained:

| # | Source | Consumer | `Match` | `Instant`/`Duration` | `IO::Path` |
|---|---|---|---|---|---|
| 1 | `builtin_type_catalog::builtin_type_info(name).mro` (`src/builtins/builtin_type_catalog.rs:492`) | `dispatch_owner_chain`/`class_chain` (**dispatch**), `e2_native_method_exists` | `Match, Capture, Cool, …` ✓ | **no row** → best-effort `[name, Any, Mu]` (`receiver_class.rs:199`) | **no row** |
| 2 | `Registry::builtin_mro_table` (`src/runtime/registry.rs:829`) + `ClassDef.mro` | `class_mro`/`class_mro_readonly` (~138 call sites) | `Match, Capture, Cool, …` ✓ | absent | registered `ClassDef` with `parents: vec![]`, `mro: ["IO::Path"]` (`runtime_init.rs:678`) ✗ |
| 3 | `builtin_type_methods::builtin_type_parents` (`src/builtins/builtin_type_methods.rs:230`) | **`.^mro`** (`classhow_mro_names`) and **`.^can`** (`collect_can_methods`) | absent → `Match, Any, Mu` ✗ | absent ✗ | absent ✗ |
| 4 | `type_matching_static.rs:189-211` `Cool` **allowlist** | smartmatch `T ~~ Cool` | absent | **present** ✓ | absent ✗ |
| 5 | `value/types_isa.rs:216` `isa_check` variant table | value-level `Cool` checks | present ✓ (**and `Capture`, which Rakudo denies**) | absent | absent |
| 6 | `value/signature.rs:1241` `is_supertype_of` | signature narrowness | absent | absent | absent |
| 7 | `resolution_method.rs:534-575` `builtin_type_distance` | multi-dispatch narrowness | absent | absent | absent |
| 8 | `dispatch_candidates.rs:800-842` specificity chains | multi-dispatch narrowness | absent | absent | absent |
| 9 | `compiler/stmt.rs:314-318` | a compile-time copy of #8 | absent | absent | absent |
| 10 | `methods_instance_ops.rs:2699` `are()` `Cool` **denylist** | `Test::Util are()` | YES (not denied) | YES (not denied) | YES (not denied) |
| 11 | `vm_call_light.rs:541` `"Any"\|"Mu"\|"Cool" => true` | light-path type check | accepts everything | accepts everything | accepts everything |
| 12 | `methods_classhow_lookup.rs:426` `[type_name, "Cool", "Any", "Mu"]` fallback | `is_builtin_type_method` (`.^can` builtin probe) | synthesised | synthesised | synthesised |

Three details make the proliferation concrete rather than theoretical:

- **Source 3 is why `.^mro` is wrong.** `classhow_mro_names` (`src/runtime/methods_classhow_mro.rs:12-23`)
  checks `registry().classes.contains_key(name)` and, for an *unregistered* builtin, goes to
  `builtin_type_parents` — a hand-written 13-entry `match` ending in `_ => &[]`. It never consults
  `builtin_mro_table`, so the correct `Match` chain sitting in sources 1 **and** 2 is unreachable
  from `.^mro`. Its own comment claims the chains "live in the single source of truth
  `builtins::builtin_type_methods`", which is exactly the belief this ADR contradicts.
- **Source 4 already records the right fact and fixed only itself.** The comment above the `Cool`
  allowlist (`src/runtime/types/type_matching_static.rs:128-135`) explicitly cites Rakudo's
  `Instant.^mro` being `((Instant) (Cool) (Any) (Mu))` — someone verified the truth, then encoded it
  in one table out of twelve. That is why `Instant ~~ Cool` is right while `Instant.^mro` is wrong.
- **`Cool` itself is registered nowhere.** It has no `ClassDef`, no `builtin_type_parents` row, no
  `builtin_mro_table` row, and no catalog row. `Cool.^mro` = `(Cool, Any, Mu)` is correct purely by
  accident of `classhow_mro_names`' `vec![class_name]` fallback plus the unconditional `Any`/`Mu`
  append.

**This proliferation, not the gate, is the architectural problem.** Any gate keyed on ancestry
inherits whichever table it happens to call, so "add a gate" is a coin flip until the tables are
one table. That is precisely what happened to the reverted attempt.

### Why the leak exists at all: the arity cascades are receiver-class-blind by construction

The other half is structural. `native_method_0arg` / `_1arg` / `_2arg` and their
`dispatch_core_*` submodules take **`&Value` only — no `&mut Interpreter`** — so they cannot
consult an MRO even in principle. Their arms are keyed on the *method name alone* and call
`target.to_string_value()` unconditionally; `Value::to_string_value`'s final `Instance` arm
(`src/value/display.rs:871`) is `format!("{}()", class_name)`, which is where `"G()"` comes from.

The codebase already knows this. `dispatch_core_str.rs`'s `chomp` arm (line 216) carries the
comment *"This layer cannot see the MRO, so route EVERY instance to the slow path"* and does
`if matches!(target.view(), ValueView::Instance { .. }) { return Some(None); }` — the one arm that
behaves correctly. The other stringifying arms do not:

- `src/builtins/methods_0arg/dispatch_core_str.rs` — `words` `:14`, `codes` `:22`, `lines` `:26`,
  `trim`/`trim-leading`/`trim-trailing` `:67`, `flip` `:76`, `chop` `:229`, `comb` `:240`,
  `join` `:349`, `fmt` `:283`
- `src/builtins/methods_0arg/dispatch_core_numeric.rs` — `uc`/`lc`/`fc`/`tc` `:362-373`
- `src/builtins/methods_0arg/dispatch_core_unicode.rs` — `chars` `:40`, `ord` `:62`, `ords` `:70`,
  the `uniname`/`unival`/… family, and a literal `_ =>` catch-all for `bytes` `:14`
- `src/builtins/methods_narg/dispatch_1arg.rs` — `index` `:449`, `substr` `:468`, `indent` `:471`;
  `dispatch_2arg.rs` — `substr` `:213`
- interpreter-side by-name dispatchers, which *do* have `&mut self`:
  `.IO` (`src/runtime/methods_dispatch_match.rs:256`) and `.subst`
  (`src/runtime/methods_string.rs:159`, whose first line is `let text = target.to_string_value();`)

The three existing name gates (`should_bypass_native_fastpath`
`src/runtime/methods_native_bypass.rs:402`, the `shadows_builtin` binding
`src/runtime/methods_call_dispatch.rs:3839`, `try_native_method_raw`
`src/vm/vm_native_dispatch.rs:356`) each hold the same
`cool_only_builtin_method(method) && class_has_wildcard_handles_or_fallback(&class_name)` term —
a narrow exception carved out for `handles *`/`FALLBACK`. `cool_only_builtin_method`
(`methods_native_bypass.rs:218`) is a hand-maintained 94-name list, itself a seventh table.

### Severity and scope

Low severity: a missing diagnostic (mutsu is too lenient), not a miscompilation. No roast test
currently depends on `G.new.uc` dying. But the *cause* is high blast radius — ancestry is read by
dispatch, `.^mro`, `.^can`, `.^methods`, multi-dispatch narrowness, and `are()` — and it is
actively misleading: two `t/handles-wildcard-builtin-methods.t` assertions are `todo`-marked
because of it, and it already cost one reverted fix attempt.

## Decision

### 1. Type ancestry has exactly one oracle

`builtin_type_catalog` (source 1) becomes the single source of truth for built-in type ancestry —
it is already the most complete and the one dispatch reads, and ADR-0019 built it for exactly this
purpose (its header currently disclaims itself as "shadow-only"; this ADR promotes it). Sources
2-9 are deleted and their consumers re-pointed at it; sources 10-12's ad-hoc `Cool` allowlist,
denylist, and synthesised fallback are replaced by an ancestry query against it. A built-in type's
parent chain is data in one table, verified against Rakudo, not a `match` arm replicated per
consumer.

The catalog gains rows for the types that have none — `Instant`, `Duration`, `IO::Path`,
`IO::Handle` — with Rakudo-verified chains. `receiver_class.rs`'s best-effort `[name, Any, Mu]`
fallback stays as a backstop for mutsu-internal types with no raku equivalent, but stops being how
four real raku types get their ancestry. `Cool` gains a row of its own rather than being correct by
accident.

**Corollary:** a type is `Cool` iff `Cool` is in that one chain. There is no `Cool` allowlist, no
`Cool` denylist, no per-consumer parent `match`, and no second opinion. A
`class_mro_includes_cool`-style helper, if it exists at all, is a one-line reader of the single
oracle.

### 2. Method existence is asked, not assumed — and an unresolved method throws

A `(receiver, method)` pair that resolves to no user method, no accessor, and no native row
**throws `X::Method::NotFound`**. It does not fall through to "stringify the receiver and apply a
`Str` method".

The existence question is already implemented: `Interpreter::e2_native_method_exists` walks
`dispatch_owner_chain` and probes the E2 row catalog. Combined with decision 1, it answers
`G.new.uc` = false (chain `[G, Any, Mu]`, and `uc` lives under `Cool`), `IO::Path.chars` = true
(chain now reaches `Cool`, row `("Cool","chars")` exists), and `DateTime.Date` = true (own row).

The `X::Method::NotFound` machinery already produces the exact Rakudo message —
`mutsu -e 'class G {}; G.new.no-such-method-at-all'` and `raku` print byte-identical output — so
correct behaviour follows from *not intercepting*, with no new error path to write.

### 3. The receiver-class decision is made where the receiver class is visible

The arity cascades stay `&Value`-only. They are pure value operations and threading an
`&mut Interpreter` into them would invert ADR-0019's direction of travel. The gate therefore lives
at the interpreter-aware call sites that already exist — the three gate sites plus the two by-name
dispatchers (`.IO`, `.subst`) — which decide **before** entering the cascade whether this receiver
may reach it.

Equivalently and preferably, this is expressed once: the ADR-0019 Phase E resolver already returns
an ordered candidate sequence for `(receiver, method)`; "no candidate" is the throw. The gate sites
are the transitional form until the resolver owns every entry point.

### 4. Data first, gate second

The phases below fix ancestry and fill the two missing rows **before** any gate is enabled. This
is the ordering the reverted attempt got backwards, and the reason it looked like six unrelated
regressions instead of one data gap.

## Phases

Each phase is independently landable and independently valuable.

- **P1 — Rakudo-verify and complete the catalog, and make `.^mro` read it.** Add `Instant`,
  `Duration`, `IO::Path`, `IO::Handle`, and `Cool` rows to `builtin_type_catalog`; correct any other
  divergence a full sweep finds. Re-point `classhow_mro_names`
  (`src/runtime/methods_classhow_mro.rs:12-23`) at the catalog, deleting `builtin_type_parents`
  (source 3) — its only non-test caller. `IO::Path` additionally needs its bootstrap `ClassDef`
  (`src/runtime/runtime_init.rs:678`) to stop declaring `parents: vec![]` / `mro: ["IO::Path"]`;
  `compute_class_mro`'s existing `parent == "Cool"` arm (`registry.rs:750`) already linearizes it.
  Pin with a test that asserts, for every catalog type, that `.^mro` equals the Rakudo chain
  (extend `tmp/mro_sweep.p6`'s comparison into a `t/` test with the raku answers baked in).
  *Alone this fixes the `Match`/`Instant`/`Duration`/`IO::Path` `.^mro` and `~~ Cool` divergences —
  user-visible on its own, no gate involved.*

  **Expect introspection counts to move, and check them deliberately.** Giving `IO::Path` a `Cool`
  ancestor makes all ~90 `("Cool", …)` rows in `native_method_row_table.rs` visible to
  `IO::Path.^can`/`.^methods` (Rakudo-correct — raku's `IO::Path.^can("chars")` is 1 where mutsu's
  is 0), and enables `Cool::`-qualified coercion, which `methods_qualified.rs:87` gates on
  `class_mro` containing `Cool`. `is_builtin_type_method` (`methods_classhow_lookup.rs:412-440`)
  carries a comment recording a *past regression of exactly this shape* — a `Pair.^can(<cool
  coercion>)` false positive caused by an unconditional `[type_name, "Cool", "Any", "Mu"]` ancestor
  list. Re-read that comment before touching source 12.

- **P2 — Collapse the remaining sources onto the catalog.** Delete `Registry::builtin_mro_table`
  and the three hardcoded narrowness chains (sources 7-9); re-point `class_mro_readonly`,
  `resolution_method`, `dispatch_candidates`, `compiler/stmt.rs`, `type_matching_static`'s `Cool`
  allowlist, `isa_check`, `is_supertype_of`, and `are_value_matches_type` at `builtin_type_info`.
  Expect this to surface real divergences (sources 7/8 carry role names like
  `Stringy`/`Positional`/`Numeric` that source 1 tracks separately in `roles`) — reconcile them into
  the catalog's `mro`+`roles` shape rather than keeping a private copy. `isa_check`'s claim that a
  `Capture` instance is `Cool` is wrong (Rakudo: `Capture, Any, Mu`) and disappears with it.

- **P3 — Fill the two genuine missing rows.** `("Instant","DateTime")` and `("Date","IO")`, both
  Rakudo-verified own methods. Audit the rest of `cool_only_builtin_method`'s 94 names against
  every catalog type for the same shape; the audit is bounded and mechanical once P1/P2 make
  ancestry trustworthy.

- **P4 — Gate the string-coercion leak.** At the three gate sites and the two by-name dispatchers,
  require `e2_native_method_exists` (or the resolver's candidate sequence) before entering the
  cascade for an `Instance` receiver. Un-`todo` `t/handles-wildcard-builtin-methods.t` tests 14-15.
  Keep `cool_only_builtin_method`'s `handles *`/`FALLBACK` term as-is — it is a *different*
  question (may an interceptor see this call) and remains correct.

- **P5 — Retire `cool_only_builtin_method`.** Once P4's existence check is authoritative, the
  94-name list is derivable: "a name with a row under `Cool` but not under `Any`/`Mu`". Deleting it
  removes the seventh table.

P1 is worth landing even if nothing else is: it is a pure data correction that fixes four types'
introspection, needs no gate, and removes the internal `~~ Cool` vs `.^mro` contradiction.

## Rejected alternatives

**Gate on `!class_mro_includes_cool` without fixing the data first.** This is the reverted attempt.
It fails because `class_mro` reads source 2, which lacks `Instant`/`Duration`/`IO::Path` entirely —
so three types that genuinely are `Cool` get gated out. The six "regressions" were the ancestry gap
becoming visible, not evidence against gating.

**Gate on row-existence alone, adding rows for every broken call.** Fixes `DateTime.Date` (row
exists) but still breaks `Instant.abs` and `IO::Path.chars` — and "adding rows" for those would
encode `Cool`'s method surface a second time under each type, which is exactly the duplication
`Cool`'s 61-row block exists to avoid. The correct statement is "`IO::Path` is `Cool`", not
"`IO::Path` has a `chars`".

**Thread `&mut Interpreter` into the arity cascades so each arm can check the MRO.** Touches every
`dispatch_core_*` arm, makes pure value operations interpreter-dependent, and pushes a dispatch
decision down into ~10 files instead of up into the ~5 sites that already have the receiver class.
It also contradicts ADR-0019's decision that arity-specific native functions are *handlers*, not
lookup entry points.

**Copy the `chomp` arm's `Instance → return Some(None)` bail into every stringifying arm.** The
cheapest patch, and it does fix the plain-class case. Rejected as a band-aid: it silently routes
every `Instance` receiver — including legitimately `Cool` ones like `IO::Path` — through the slow
path, trading a correctness bug for a performance cliff on real `Cool` types, and it leaves all six
ancestry tables in place. It would also have to be re-applied to each new stringifying arm forever.

**Leave it: severity is low and no roast test depends on it.** The severity of the *symptom* is
low, but the cause is six disagreeing ancestry tables that are read by dispatch, introspection, and
multi-dispatch. It has already produced one wrong `.^mro`, one wrong `~~ Cool`, two `todo`-marked
tests, and one reverted PR. That is an architectural debt, not a diagnostic nicety.

## Consequences

- Four built-in types report correct `.^mro`/`.^can`/`~~ Cool` after P1; the `~~ Cool` vs `.^mro`
  self-contradiction disappears.
- Twelve ancestry tables become one; adding a built-in type stops meaning "remember to update
  eleven other `match` arms".
- Plain classes stop answering 26 string methods they do not have, and `.^can` stops contradicting
  dispatch.
- P2 is the risky phase (multi-dispatch narrowness reads sources 7-9), and is where CI/roast is the
  safety net — the reconciliation of `mro` vs `roles` between the tables is the part to expect
  failures from.
- `Value::to_string_value`'s `Instance` arm (`"{class_name}()"`) stays as-is. It is the right answer
  for `.gist`-adjacent uses; the bug was calling it from methods that should never have run. Note
  separately that it ignores a user-declared `method Str` (`S.new.uc` gives `"S()"`, not `"hello"`),
  which P4 makes moot for non-`Cool` receivers but which remains worth checking for `Cool`-derived
  user classes.

## Verification performed for this ADR

All on `main` @ `47e081026`, `target/debug/mutsu` vs system `raku`:

- `tmp/cool_sweep.p6` — 60 `Cool` names on `class G {}`; 26 divergences, all string-family.
- `tmp/mro_sweep.p6` — 27 types, `.^mro` and `~~ Cool` in both implementations; exactly four
  disagree with Rakudo, three of those disagree with themselves.
- `.^can` probes — `G.^can("uc")` = 0 in both; `Instant.^can("abs")` = 0 in mutsu vs 2 in raku;
  `IO::Path.^can("chars")` = 0 vs 1; `Date.^can("IO")` = 0 vs 1; `DateTime.^can("Date")` = 1 in both.
- `X::Method::NotFound` message parity confirmed byte-identical.
- `native_method_row_exists` / `e2_native_method_exists` confirmed present and `pub(crate)`,
  contradicting the todo file's "would need to be built and exposed".
