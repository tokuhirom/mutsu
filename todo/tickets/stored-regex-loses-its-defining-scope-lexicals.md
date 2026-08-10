# A stored regex loses its defining scope's lexicals when it escapes the sub

Extracted from PLAN.md §1 B4 (2026-08-02) and re-verified on `main` the same day. Originally found
while running the (now-retired) Tubu web framework; it is a general interpreter bug on its own axis.

## Repro

```raku
sub make() {
    my $word = 'abc';
    return rx/ $word /;
}
my $r = make();
say ("xx abc yy" ~~ $r).defined;   # mutsu: False    raku: True
```

The regex is built inside `make`, interpolates the lexical `$word`, and is then returned. By the
time it runs, mutsu no longer resolves `$word`, so the match fails. `b07ee6627`
("feat(runtime): a regex literal captures its defining scope") fixed the same-scope case; this
escaping-the-frame case still fails, so the capture is not surviving the frame teardown.

## Second, related divergence in the same area

A stored regex used as a `<$var>` assertion leaks its inner captures into the *outer* match's
positional slots:

```raku
my $inner = rx/ (\d+) /;
my $outer = rx/ 'n=' <$inner> /;
"n=123" ~~ $outer;
say ($/[0] // 'undef');   # mutsu: ｢123｣     raku: undef
```

In Rakudo a `<$var>` assertion does not publish the sub-regex's positional captures into the calling
match (both agree that `$/{'$inner'}` is undefined). mutsu splices them in as `$/[0]`.

## Affected files

`src/runtime/regex.rs` / `regex_parse*.rs` (assertion handling for `<$var>`), and whatever performs
the defining-scope capture for a regex literal (see `b07ee6627` and
`news/2026-08/regex-literal-captures-defining-scope.md` if present).

## Why it is not a one-liner

The first half is the closure-capture-lifetime family (the captured env must outlive the frame that
built the regex, like a `Sub`'s closure), and the second half is capture-namespace plumbing through
assertion invocation — two different mechanisms that happen to meet in the same construct.

## Deep-dive investigation (2026-08-10)

**Recommendation: split this into two tickets / two PRs.** The two halves have disjoint root causes,
touch disjoint files, and neither depends on the other. Bug 1 (defining-scope capture) lives in the
compiler trigger + `Value::RegexCaptured` machinery from `b07ee6627`; bug 2 (capture leak) lives in
the regex tokenizer/interpolator. The plans below are structured accordingly. Both were re-verified
on `main` (post-#6174) on 2026-08-10; all raku columns below were produced with the system `raku`
the same day.

### Bug 1 root cause — the closure capture only fires for *code-bearing* patterns, and is a stale by-value snapshot even then

`b07ee6627` added `Value::RegexCaptured` (`src/value/mod.rs`, `RegexClosure { pattern, scope:
Arc<HashMap<String, Value>> }`) loaded via `OpCode::LoadRegexClosure`. But the compiler trigger,
`regex_literal_closure_captures()` at **`src/compiler/expr_helpers.rs:711`**, bails at **line 717**:

```rust
if !pattern.contains('{') && !pattern.contains(":my ") && !pattern.contains(":let ") {
    return None;
}
```

So `rx/ $word /` (no embedded code) compiles to a plain `LoadConst` — **nothing captures the
defining scope at all**. To answer the design question directly: for plain interpolation the stored
regex holds **(b) names resolved against the live env at match time**; for code-bearing patterns it
holds **(a) a by-value snapshot**. Neither is Raku's semantics (closure over the defining scope's
*bindings*).

The match-time lookups that then return the wrong/absent value (a stored regex is re-parsed on every
match, so "parse time" here *is* match time):

- Bare `$name` / `${name}` interpolation: `interpolate_regex_scalars()` at
  **`src/runtime/regex_parse_modifier.rs:355-360` and `:423-426`** — `self.env.get(name)` on the
  match-site env; the defining frame is gone, lookup yields `Value::NIL`, and
  `push_value_as_regex_pattern` (**`regex_parse_modifier.rs:641`**) substitutes `<!>`
  (never-match) for Nil. Hence the silent match failure in the main repro.
- `<$var>` assertion: tokenizer at **`src/runtime/regex_parse_core.rs:2521`** —
  `self.env.get(var_name)`, and the `None` arm throws `X::Undeclared` ("Variable '$pat' is not
  declared"), which is what the `<$pat>`-flavoured repro dies with.
- The undeclared-vars pre-check at **`regex_parse_modifier.rs:618`** has the same env dependency.

**The snapshot half is also wrong, proven by probe W5/W6 below**: `install_regex_closure_scope()`
(**`src/runtime/seq_helpers/smart_match.rs:75-92`**) inserts the *snapshot values* into env for the
match, shadowing the live (mutated) lexical. Raku evaluates regex-embedded code and interpolation at
match time against the defining *binding*, so mutation after regex construction must be visible.
mutsu today fails `my $x = 1; my $re = rx/ abc <?{ $x == 2 }> /; $x = 2; "abc" ~~ $re` (mutsu:
False, raku: True) — even in the same scope. So bug 1's fix cannot be "widen the trigger" alone;
the capture must hold **shared cells** (`ContainerRef`), exactly the mechanism closures use
(`box_captured_lexicals`, `src/vm/vm_register_ops.rs:759`) and exactly what the CLAUDE.md "gain"
section prescribes. Note `capture_regex_closure()` (**`src/vm/vm_register_ops.rs:29`**) already
documents "a capture that is already a shared `ContainerRef` cell is kept AS the cell" — the cell
transport works; what is missing is (i) the widened trigger and (ii) making the mutated captures
*be* cells.

### Bug 2 root cause — `<$var>` splices the inner pattern's capture groups into the outer capture numbering

The tokenizer's `<$var>` arm at **`src/runtime/regex_parse_core.rs:2519-2596`** resolves the
variable, extracts the inner regex's **pattern string**, re-parses it, and wraps it as
`RegexAtom::Group(parsed)` (line 2593). A `Group` is transparent to capture accumulation: the inner
pattern's `CaptureGroup` atoms (matched at **`src/runtime/regex/regex_match_capture.rs:220`**, which
pushes a `PosSlot` into the caps the caller merges) and its `named_capture` tokens are numbered/named
straight into the **outer** match. Raku gives a `<$var>` call its own discarded Match object — no
positional *or named* leakage (probes W1/W2 below; mutsu leaks both).

There are three leak sites, all in the "value holds a Regex" family:

1. `<$var>` → `RegexAtom::Group` — `regex_parse_core.rs:2590-2596` (the main one).
2. Bare `$var` / `${name}` interpolation of a Regex value — `push_value_as_regex_pattern`
   (**`regex_parse_modifier.rs:642-643`**) splices the raw pattern *text* (parens included) into
   the outer pattern before parsing, so the inner `(\d+)` literally becomes an outer capture group.
3. `@var` / `<@var>` alternation with Regex elements — text splice at
   **`regex_parse_modifier.rs:496-505` and `:536-545`**, atom path `array_var_alternation_atom`
   (**`regex_parse_core.rs:245`**).

### Variant table (raku vs mutsu, 2026-08-10, all reproduced)

| # | Probe | raku | mutsu |
|---|-------|------|-------|
| B1 | `sub make { my $word = 'abc'; rx/ $word / }` matched after return | True | **False** |
| V1 | same scope: `$pat='abc'; $re=rx/$pat/; $pat='zzz'` — match `abc` / `zzz` | False / True (match-time value) | False / True (accidental: live env) |
| V2 | two calls of `mk($w)` returning `rx/$word/` — each keeps its own | True / True / False | **False / False** / False |
| V3 | regex per loop iteration `for <one two> -> $w { push rx/$w/ }` | each keeps its iteration | **False / False** |
| V4 | nested sub, regex stored in hash, matched later | True | **False** |
| V5 | `<$pat>` form escaping the sub | True | **dies X::Undeclared** |
| V6 | `<$pat>` + mutation: match-time value wins | False / True | (dies) |
| W5 | `my $x=1; $re=rx/abc <?{ $x==2 }>/; $x=2` same-scope | True | **False** (stale snapshot) |
| W6 | code-bearing, mutated before `return` | True | **False** (stale snapshot) |
| W1 | `<$inner>` where inner has `$<d>=(\d+)`: outer `$/<d>` / `$0` | undef / undef | **｢123｣** / undef |
| B2 | `<$inner>` where inner has `(\d+)`: outer `$0` | undef | **｢123｣** |
| W2 | bare `$inner` (Regex value) interpolated: outer `$0` | undef | **｢123｣** |
| W3 | `<alias=$inner>`: `$/<alias>` / its `[0]` / `$0` | ｢123｣ / ｢123｣ / undef | **undef** / — / undef (separate pre-existing gap) |
| W4 | `<@pats>` with capturing Regex elements: outer `$0` | undef | **｢123｣** |
| W7 | `~$m` after `/ 'n=' <$inner7> /` (consumption) | `n=123` | `n=123` (agrees) |

Repro scripts kept the session: `tmp/rx-variants.raku`, `tmp/rx-variants2.raku`.

### Fix plan — bug 1 (PR 1): regex literals capture their defining scope as shared cells

Everything reuses the `b07ee6627` machinery; no new Value variant, no new opcode.

1. **Widen the compiler trigger** — `src/compiler/expr_helpers.rs:717`: delete the code-bearing
   gate (`if !pattern.contains('{') && … return None;`). Any pattern whose
   `CompiledCode::regex_interpolated_var_names()` (src/opcode.rs:4880) yields at least one
   resolvable name now loads through `LoadRegexClosure`. Keep the existing filters (`_`, `/`,
   all-digit names). The name scanner already skips `$<name>`, `$0`, and twigils (`$*x`) because
   the char after the sigil must be alphabetic/underscore — verify with a unit test, do not change
   it.
2. **Track which captured names are mutated in the defining frame** — in
   `CompiledCode::finalize`'s free-var analysis (src/opcode.rs, the regex-constants scan around
   line 5335: `for c in &self.constants { … regex_interpolated_var_names … }`): today a name that
   IS one of `own`'s locals is simply skipped. Additionally collect those own names into a local
   `regex_captured_own: HashSet<Symbol>`. After the nested-closure fold has finished growing
   `self_mutated` (i.e. just before the assignments at src/opcode.rs:5516), store
   `regex_captured_own ∩ self_mutated` into a **new field**
   `pub(crate) needs_cell_regex: Vec<Symbol>` on `CompiledCode` (initialize alongside
   `needs_cell_locals` at :4029/:5517). Do NOT fold into `needs_cell_locals` — that set feeds
   `box_captured_lexicals`' closure paths and the named-sub decl-site boxing deliberately avoids
   it (see the over-boxing warning at src/vm/vm_var_assign_set_local.rs:239-242).
3. **Box at capture time** — in `capture_regex_closure()` (src/vm/vm_register_ops.rs:29): before
   reading a capture whose sym is in `code.needs_cell_regex` and whose slot is a real local
   (`slot != NOT_A_LOCAL`), box the slot into a shared cell by calling the existing
   `box_decl_local_cell(code, slot as usize)` (src/vm/vm_var_assign_local_get.rs:316 — it already
   applies the correct skips: scalars only, no re-boxing, skip reference-bearing values and
   type/`where`-constrained scalars, and it mirrors the cell into env). Then read the slot as
   today — the captured value IS the cell, later frame writes go through it, and the stored regex
   sees them. Unmutated captures stay cheap by-value snapshots (correct: nothing can change them).
   A typed/constrained mutated scalar silently stays a snapshot — same accepted limitation as
   closures; leave a `// TODO` noting it.
4. **Deref at the `<$var>` consumption site** — `src/runtime/regex_parse_core.rs:2521`: after the
   env lookup add `.map(|v| v.into_deref())` (the bare-`$name` interpolation path already derefs at
   regex_parse_modifier.rs:361/:427; the `<$var>` arm does not, and a cell would otherwise
   stringify). Also add the `or_else(|| self.env.get(&format!("${var_name}")))` fallback the other
   lookups have, for keying symmetry.
5. **Audit the install sites** — `install_regex_closure_scope` is already wrapped around `~~`
   (smart_match.rs:237) and `.match`/`.subst` (methods_dispatch_match.rs:225/:228). Grep for other
   entry points a *stored* regex value can reach — at minimum the `s///`/`TR///` VM ops in
   `src/vm/vm_string_regex_ops.rs` and `.split`/`.comb` with a regex argument — and wrap them with
   `with_regex_closure_scope` (smart_match.rs:122) where the regex flows in as a runtime value.
   (They were not needed for code-bearing patterns' tests, but plain interpolation makes far more
   regexes carry scopes.)
6. **Do not touch** `install_regex_closure_scope`'s shadowing order: with cells, the installed
   binding and the live frame binding are the *same cell*, which is what makes W5/V1 come out
   right; the existing "rebound bindings survive uninstall" logic (smart_match.rs:99-117) is
   unaffected.

Expected behavioral deltas beyond the repro: V2/V3/V4/V5/W5/W6 all flip to raku's column. One
deliberate semantic change: a regex interpolating `$x` matched in a scope with a *different* live
`$x` now uses the defining scope's binding (raku-correct; previously match-site env won). Perf
note: every interpolating regex literal now allocates a small scope map at load and pays an env
install/uninstall per match — watch the bench CI row, but numeric benches (fib etc.) contain no
regexes and are unaffected.

### Fix plan — bug 2 (PR 2): `<$var>`-family calls are capture-isolated

Mechanism choice: **parse-time capture stripping**, not a new `RegexAtom` variant. The engine has
many `match atom` sites (regex_match_atom.rs, regex_match_atom_simple.rs, regex_match_capture.rs,
regex_casefold.rs, regex_helpers.rs, plus zero-width/count classifiers) and a new variant would have
to be threaded through all of them; a recursive pattern transform needs one function and no engine
changes. The cost is one accepted edge: an inner-pattern backreference to an inner capture
(`rx/(\w)$0/` used via `<$var>`) stops working — rare, note it with a `// TODO` (fixing it properly
needs match-time isolation, i.e. the atom-variant design).

1. **Add `strip_captures_pattern(&RegexPattern) -> RegexPattern`** in
   `src/runtime/regex/regex_helpers.rs`, modeled byte-for-byte on the `strip_marks_pattern` /
   `strip_marks_token` / `strip_marks_atom` traversal at :503-:590 (it already recurses through
   every composite atom: Group, CaptureGroup, Alternation, SequentialAlternation, Conjunction,
   Lookaround, GoalMatch, and the token `separator`). The transform: `CaptureGroup(p)` →
   `Group(strip(p))`; on every token clear `named_capture`, `secondary_named_capture`,
   `hash_capture`, and `force_list_capture` (struct fields at src/runtime/regex_types.rs:333-356);
   recurse everywhere else.
2. **Apply at the `<$var>` arm** — `regex_parse_core.rs:2590-2596`: wrap the parse result:
   `RegexAtom::Group(strip_captures_pattern(&parsed))`. This fixes B2 and W1 (named leak) at once.
   W3 (`<alias=$inner>` should expose the whole match under `$<alias>` *with* inner subcaptures) is
   a separate pre-existing gap — mutsu yields undef there today, and stripping does not change
   that; leave it out of scope with a note.
3. **Bare `$var` / `${name}` holding a Regex** — in `interpolate_regex_scalars`
   (regex_parse_modifier.rs, the two `push_value_as_regex_pattern` calls at :363 and :429): when
   `value.view()` is `Regex`/`RegexWithAdverbs`, emit the text `<$name>` instead of splicing the
   pattern body. The tokenizer's `<$var>` arm (fixed in step 2) then handles it — same env, same
   match invocation, so the lookup re-resolves identically, and the W7 consumption semantics are
   preserved. Non-regex values keep the existing escaped-literal splice. Do NOT touch the
   bound-params variant `interpolate_bound_regex_scalars` (regex_interpolate.rs:288) — its values
   live in a binding overlay the tokenizer cannot see; audit it separately if a leak is ever
   reproduced through it.
4. **Array alternation** — `array_var_alternation_atom` (regex_parse_core.rs:245): apply
   `strip_captures_pattern` to each element parsed from a Regex value. For the *text* alternation
   paths (regex_parse_modifier.rs:496-505 and :536-545): when any element is a Regex value, emit
   `<@name>` (the tokenizer path) instead of the textual alternation; string-only arrays keep the
   current text path unchanged. This fixes W4.
5. The Junction arm of `push_value_as_regex_pattern` (:644-657) has the same theoretical leak for
   `any(rx/(a)/, …)`; out of scope — note with a TODO.
6. **Known limitation to document in the PR**: a `RegexCaptured` inner value reached via `<$var>`
   loses its *own* captured closure scope (only the pattern string is extracted at
   regex_parse_core.rs:2546-2549). After bug 1 lands, an escaped-and-nested regex may still miss
   its lexicals; fixing that needs the inner scope installed around the Group match (the
   atom-variant design). Leave a `// TODO` at the extraction site.

### Inline test file (add as `t/regex-stored-closure-scope.t` in PR 1, extend `plan` and add the bug-2 block in PR 2)

```raku
use v6;
use Test;

plan 17;

# --- Bug 1: a stored regex keeps its defining scope ---------------------------

sub make() { my $word = 'abc'; return rx/ $word /; }
my $r = make();
ok ("xx abc yy" ~~ $r).defined, 'escaped regex still resolves its lexical (main repro)';

sub mk($w) { my $word = $w; return rx/ $word /; }
my $ra = mk('aaa');
my $rb = mk('bbb');
ok ("aaa" ~~ $ra).defined, 'first call keeps its own value';
ok ("bbb" ~~ $rb).defined, 'second call keeps its own value';
nok ("bbb" ~~ $ra).defined, 'calls do not share a frame (snapshot-vs-shared-frame discriminator)';

my @res;
for <one two> -> $w { @res.push: rx/ $w /; }
ok ("one" ~~ @res[0]).defined, 'loop iteration 0 keeps its value';
ok ("two" ~~ @res[1]).defined, 'loop iteration 1 keeps its value';
nok ("two" ~~ @res[0]).defined, 'iterations do not share';

my %h;
sub outer { my $x = 'qq'; sub inner { return rx/ $x /; }; %h<r> = inner(); }
outer();
ok ("a qq b" ~~ %h<r>).defined, 'nested sub, stored in a hash, matched later';

sub mk5 { my $pat = 'ab'; return rx/ <$pat> /; }
ok ("xaby" ~~ mk5()).defined, '<$var> assertion form survives the frame';

# Match-time evaluation: mutation after construction is visible (raku-verified).
{
    my $pat = 'abc';
    my $re = rx/ $pat /;
    $pat = 'zzz';
    nok ("abc" ~~ $re).defined, 'interpolation sees the mutated value, not a snapshot (1)';
    ok  ("zzz" ~~ $re).defined, 'interpolation sees the mutated value, not a snapshot (2)';
}

# The code-bearing capture must be a live cell, not a stale snapshot (W5/W6).
{
    my $x = 1;
    my $re = rx/ abc <?{ $x == 2 }> /;
    $x = 2;
    ok ("abc" ~~ $re).defined, 'embedded code sees a same-scope mutation after construction';
}
sub mk6 { my $w = 'no'; my $r2 = rx/ abc <?{ $w eq 'yes' }> /; $w = 'yes'; return $r2; }
ok ("abc" ~~ mk6()).defined, 'embedded code sees a mutation made before the frame died';

# --- Bug 2: <$var>-family calls are capture-isolated (raku-verified) ----------

{
    my $inner = rx/ $<d>=(\d+) /;
    my $m = "n=123" ~~ / 'n=' <$inner> /;
    is ~$m, 'n=123', '<$inner> still consumes its text';
    nok $m[0].defined,   '<$var> does not leak positional captures into $/';
    nok $m<d>.defined,   '<$var> does not leak named captures into $/';
}
{
    my $inner2 = rx/ (\d+) /;
    "n=123" ~~ / 'n=' $inner2 /;
    nok $0.defined, 'bare $var regex interpolation does not leak captures either';
}
```

(A `<@pats>`-leak assertion — `my @pats = rx/(\d+)/, rx/(x+)/; "n=123" ~~ / 'n=' <@pats> /;
nok $0.defined` — belongs in the PR-2 file too; bump the plan accordingly.)

### Regression hazards

- **`t/regex-literal-is-a-closure.t`** — the 9-subtest pin for `b07ee6627`. Subtest 2 ("the
  defining scope wins over the match-site lexical") is the one a careless install-order change
  breaks; with cells it must still pass (the cell IS the defining binding). Subtests 4/5 pin that
  embedded-code writes survive uninstall.
- **`t/gate-b-regex-interp-env-sync.t`**, **`t/regex-array-var-lookahead.t`**,
  **`t/match-subst-named-regex-arg.t`**, **`t/regex-alias-subrule-captures.t`** — interpolation /
  env-sync / alias-capture behavior adjacent to both fixes.
- Whitelisted roast spot-checks (run with `MUTSU_FUDGE=1 prove -e 'target/debug/mutsu'`):
  **`roast/S05-interpolation/regex-in-variable.t`** and **`roast/S05-interpolation/lexicals.t`**
  (exactly this feature area — they pass today and must stay green),
  **`roast/S05-capture/subrule.t`** (capture-through-assertion semantics, sensitive to the strip
  transform), **`roast/S05-metasyntax/regex.t`** (broad `<...>` metasyntax coverage). Also worth a
  look: `roast/S05-capture/caps.t`.
- Bug 1 step 2 touches `CompiledCode::finalize`'s free-var analysis — the env-writeback /
  ADR-0018 family is historically sensitive to it (see MEMORY "env-writeback campaign"); keep the
  change purely additive (a new set, no changes to `free`/`captured_mutated` membership).
- Bug 2 step 3 changes what text the outer pattern contains; anything that caches by pattern
  string (`regex_token_resolve.rs` with-args cache) should be sanity-checked with a grammar-heavy
  file, e.g. `roast/S05-grammar/parse_and_parsefile.t`.
