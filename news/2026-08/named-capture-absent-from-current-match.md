# `$<name>` for a capture absent from the current match leaks the previous match's value

**Resolved.** Implemented per the fix plan below: a new
`Interpreter::reset_capture_env_vars` helper (`src/runtime/seq_helpers/regex_captures.rs`)
purges stale numeric (Nil-out) and named (`remove_sym`) capture env keys, called
at every top-level `$/`-install site (`clear_match_state`,
`apply_single_regex_captures`, `apply_multi_regex_captures`, both
`smart_match_inner` regex arms, `dispatch_match_method`'s multi- and
single-match branches, `exec_subst_op`/`exec_non_destructive_subst_op`,
`try_native_subst`'s regex branch, and `dispatch_package_parse`). A second,
smaller fix was needed alongside it: `$<name>` after a *failed* match (where
`$/` is `Nil`) fell through to calling `AT-KEY` on `Nil`, which answers an
undefined `Any` type object (Nil's ordinary Cool-autoboxing behavior) rather
than `Nil` itself — `exec_get_capture_var_op`
(`src/vm/vm_misc_codevar.rs`) now short-circuits `ValueView::Nil` to
`Value::NIL` directly instead of round-tripping through `AT-KEY`. All 7
verification probes and the 14-assertion regression test
(`t/regex-stale-named-capture-cleared.t`) now match raku exactly. Verified
against the whitelisted roast S05-capture/S05-match/S05-grammar/S05-modifier
suites (18 files, 477 tests) and the full local `t/` suite (3004 files,
28189 tests) with no regressions.

Found while writing a regression test for
`news/2026-08/regex-token-named-optional-atom-empty-match-not-nil.md`
(the `?`-on-the-named-token fix).

`$<name>` (equivalently `$/<name>` / `$<name>` after `~~`) should be `Nil` when
the *current* successful match's pattern has no capture called `name` at all
-- not merely when the name matched zero times. mutsu instead returns the
value from an *earlier* match in the same dynamic scope:

```raku
if "xb" ~~ / $<x>=<[cdx]> "b" / {
    say "block1 x=", ~$<x>;      # x           -- both agree
}
if "bb" ~~ / "b" "b" / {         # this pattern has no $<x> at all
    say "block2 x=", $<x>.WHAT;  # raku: Nil : mutsu: (Match), the stale "x" from block1
}
```

raku: `block1 x=x` / `block2 x=Nil`.
mutsu: `block1 x=x` / `block2 x=(Match)` (leaking block1's captured Match).

Reproduces the same way through a plain top-level `sub` call, so it is not
specific to bare-block topicalization:

```raku
sub m1 { "xb" ~~ / $<x>=<[cdx]> "b" / }
sub m2 { "bb" ~~ / "b" "b" / }
m1();
m2();
say $<x>.WHAT;   # raku: Nil, mutsu: (Match)
```

## Why this matters

Any code that branches on `$<name>.defined` after a *second* match whose
pattern doesn't declare `name` will silently see a stale Match object instead
of Nil. This is a narrower, unrelated bug from the "which `?` placement gives
Nil vs an empty Match" question that
`news/2026-08/regex-token-named-optional-atom-empty-match-not-nil.md` fixed --
that fix is specifically about names *present* in the current pattern that
matched zero times; this ticket is about names *absent* from the current
pattern's token list entirely.

## Where to look

The named-capture read path for `$<name>` / `$/<name>` (`Match` indexing by
Str key) almost certainly resolves through some `$/`-adjacent lookup that,
when the current match's `named` map has no entry for the key, falls back to
a broader/older store instead of returning `Nil` outright. Candidates:
`src/runtime/regex/regex_match_public.rs`, the `Match` hash-subscript method
implementation, and wherever the interpreter installs `$/` after a successful
top-level `~~`/`.match` (look for where the *previous* `$/`'s named map could
still be reachable when building the new one).

## Effort

Not measured; likely S-M once the actual lookup site is found, but requires
tracing the `$/`-installation path fresh (not touched by the ticket that
found this).

## Deep-dive investigation (2026-08-10)

Root cause confirmed. The ticket's guess ("a `$/`-adjacent lookup falls back
to a broader/older store") is right in spirit but the mechanism is the
opposite direction: the stale value is not a *fallback*, it *shadows* the
correct `$/`-based lookup. Everything below was verified on current `main`
(52f217429) against system `raku`.

### Confirmed storage model

mutsu keeps **three parallel stores** for match state, all in the
interpreter's single dynamic env (NOT per-frame; see the scoping note below):

1. **`$/`** = env key `"/"`. Installed (as a Match instance, a List for
   `:g`/`:ov`/`:ex`, or `Value::NIL` on failure) by every top-level match
   path.
2. **Positional captures** = env keys `"0"`, `"1"`, ... . `$0` parses as
   plain `Expr::Var("0")` (verified via `--dump-ast`), i.e. an ordinary env
   read with **no** `$/` fallback. That is why the install sites *Nil-out*
   stale numeric keys rather than removing them.
3. **Named captures** = env keys `"<name>"` (angle brackets included in the
   key). `$<x>` parses as `Expr::CaptureVar("x")`
   (`src/parser/primary/var/scalar.rs:211`), compiles via
   `compile_expr_capture_var` (`src/compiler/expr_data.rs:89-103`) to
   `OpCode::GetCaptureVar` with the string constant `"<x>"`.

**Lookup site** — `exec_get_capture_var_op`,
`src/vm/vm_misc_codevar.rs:15-45`:

- First tries `self.env().get("<x>")` — if the key is present (any value),
  that wins.
- Only if the key is **absent** does it fall back to reading `$/` (local
  slot `"/"` first, then env `"/"`) and doing an `AT-KEY` lookup on it,
  defaulting to `Value::NIL`.

The env-first order is deliberate and load-bearing: mid-match code blocks
(`/ $<x>=[a] { say ~$<x> } b /`) see `<x>` via incremental deposits from
`src/runtime/regex/regex_eval_repeat.rs:367` and `:652` before the final
`$/` is installed, and grammar action methods rely on the local-slot-`"/"`
fallback surviving nested regex ops (see the comment in
`vm_misc_codevar.rs:20-26`, pinned by `t/capture-var-topic-slot.t`). So the
per-name store cannot simply be deleted (fix shape (a) is out).

**Deposit / `$/`-install sites** (top-level, i.e. the ones that persist
after the match):

- Plain `~~ /re/` (the ticket repro): `smart_match_inner` single-regex arm,
  `src/runtime/seq_helpers/smart_match.rs:864-988` — resets stale numeric
  keys at :889-900, deposits named keys at :976-983, installs `"/"` at
  :984. **No stale `<name>` purge.**
- `~~ &token` (named token RHS): same file, arm at :411-485 — deposits
  numerics at :441-444, `"/"` at :455, named at :456-471. **No numeric
  reset and no `<name>` purge.**
- `apply_single_regex_captures`,
  `src/runtime/seq_helpers/regex_captures.rs:36-130` (used by `:nth`, `:c`,
  `:pos`, P5 single match, ...) — installs `"/"` at :100, resets numerics at
  :102-111, deposits named at :123-129. **No `<name>` purge.**
- `apply_multi_regex_captures`, same file :278-313 (`:g`/`:ov`/`:ex`) —
  installs `"/"` list at :305, sets numerics from the first match at
  :306-312. **No numeric reset and no `<name>` purge.**
- `.match` method: `dispatch_match_method`,
  `src/runtime/methods_match_dispatch.rs` — multi-match early install at
  :190, single-match deposits at :215-229. **No resets at all.**
- `s///` (VM): `exec_subst_op` (`src/vm/vm_subst_exec.rs:7`) and
  `exec_non_destructive_subst_op` (`:290`) install `"/"` at ~20 sites in
  that file (:57, :239, :341, ...); the native fast path is
  `try_native_subst` / `native_subst_regex` in `src/vm/vm_native_subst.rs`
  (:130, :179, :183). **No `<name>` purge anywhere.**
- `Grammar.parse`: `dispatch_package_parse`,
  `src/runtime/methods_grammar.rs:594-601` — deposits named at :596-599,
  installs `"/"` at :601. **No `<name>` purge.**
- **Failure path**: `clear_match_state`,
  `src/runtime/seq_helpers/regex_captures.rs:7-18` — Nils `"/"` and all
  numeric keys. **Does NOT touch `<name>` keys.**

Sites that are already clean and must NOT be touched:

- Grammar *action* machinery (`methods_grammar.rs:875-1012` and
  `:1289-1483`) snapshots all current `<...>` keys, `remove_sym`s them,
  deposits its own, and restores the snapshot afterwards — self-cleaning.
- Mid-match incremental deposits (`regex_eval_repeat.rs:367/:652`) and the
  fresh eval env built by `make_regex_eval_env`
  (`src/runtime/regex/regex_resolve.rs:529-575`) — in-match machinery, out
  of scope.
- `eval_subst_replacement_cased` (`src/runtime/methods_string_subst_repl.rs:10`)
  clones and restores the whole env around the replacement block (:41), so
  its deposits do not persist — not a leak source.

**So the bug is:** numeric keys are reset on every install, `<name>` keys
never are, and a stale `<name>` env entry shadows the `AT-KEY`-on-`$/`
fallback in `GetCaptureVar` forever after.

### Verified behavior table (raku vs mutsu main, 2026-08-10)

| Probe | raku | mutsu main |
|---|---|---|
| `"xb" ~~ /$<x>=<[cdx]> "b"/;` then `"bb" ~~ /"b" "b"/;` → `$<x>.WHAT` | `Nil` | **`(Match)` (stale)** |
| same, `$/.hash` after 2nd match | empty `Map` | empty `{}` (already correct — proves `$<x>` does not read `$/`) |
| `"ab" ~~ /a(b)/;` then `"cd" ~~ /cd/;` → `$0.raku` | `Nil` | `Nil` (numerics already reset — not affected) |
| `$<x>` / `$0` before any match ever ran | `Nil` / `Nil` | `Nil` / `Nil` (correct) |
| `"ab" ~~ /$<x>=[ab]/;` then **failed** `"zz" ~~ /q/;` → `$<x>.WHAT`, `$/.WHAT` | `Nil`, `Nil` | **`(Match)` (stale)**, `Nil` |
| stale `$<x>`, then `"cd".match(/cd/)` | `Nil` | **`(Match)`** |
| stale `$<x>`, then `$s ~~ s/c/X/` | `Nil` | **`(Match)`** |
| stale `$<x>`, then `"cd" ~~ &t` (token RHS) | `Nil` | **`(Match)`** |
| stale `$<x>`, then `G.parse("cd")` | `Nil` | **`(Match)`** |
| stale `$<x>`, then `"cd" ~~ m:g/cd/` | `Failure` (list `$/`) | **`(Match)`** |

**Failed-match rule (pinned by measurement):** rakudo (6.d, this machine)
sets `$/` to `Nil` after a failed `~~` — it does **not** preserve the last
successful match's `$/`. mutsu's `clear_match_state` already matches that
for `"/"` and numerics; only the `<name>` keys leak.

**Scoping note:** in raku, `$/` is a fresh per-routine lexical (`sub f {
say $/.WHAT }` after a file-scope match prints `Nil`; a match inside a sub
does not touch the caller's `$/`). In mutsu it is a single dynamic env slot
visible everywhere (the same probe prints `(Match)`). That divergence is
**out of scope** for this ticket — the fix below operates on the single
env store, and the ticket's sub-call repro (`m1(); m2(); say $<x>.WHAT`)
converges to `Nil` under it in both implementations.

### Chosen fix — (b): purge stale `<name>` env keys at every top-level `$/` install

Fix shape (a) (route `$<name>` exclusively through `$/`) is rejected: the
env-first order in `GetCaptureVar` is required by mid-match code blocks and
by the action-method nested-match scenario documented in
`vm_misc_codevar.rs:20-26` / `t/capture-var-topic-slot.t`. Fix (b) mirrors
the numeric-key reset that already exists at the install sites.

**Critical detail — purge means `remove_sym`, NOT insert-Nil.**
`exec_get_capture_var_op` short-circuits on a *present* key even if its
value is Nil; a Nil entry would therefore shadow the local-slot-`"/"`
fallback that action methods depend on (breaking
`t/capture-var-topic-slot.t`). Numeric keys keep their existing Nil-insert
treatment (`$0` is a plain `Var` read with no fallback to shadow).

Step-by-step:

1. **Add one helper** in
   `src/runtime/seq_helpers/regex_captures.rs` (inside `impl Interpreter`),
   declared `pub(crate)` so both `crate::runtime` and `crate::vm` call it:

   ```rust
   /// Reset capture env vars left over from a previous match: numeric keys
   /// (`0`, `1`, ...) are set to Nil ($0 is a plain env `Var` read), and
   /// named-capture keys (`<name>`) are REMOVED so `$<name>`
   /// (OpCode::GetCaptureVar) falls through to the current `$/` AT-KEY
   /// lookup instead of seeing a stale entry. Removal, not Nil-ing, is
   /// load-bearing: a present-but-Nil entry would shadow the local-slot
   /// `$/` fallback action methods rely on (t/capture-var-topic-slot.t).
   pub(crate) fn reset_capture_env_vars(&mut self) {
       let numeric_keys: Vec<Symbol> = self
           .env
           .keys()
           .filter(|k| k.with_str(|s| !s.is_empty() && s.chars().all(|c| c.is_ascii_digit())))
           .copied()
           .collect();
       for key in numeric_keys {
           self.env.insert_sym(key, Value::NIL);
       }
       let angle_keys: Vec<Symbol> = self
           .env
           .keys()
           .filter(|k| k.with_str(|s| s.len() > 2 && s.starts_with('<') && s.ends_with('>')))
           .copied()
           .collect();
       for key in angle_keys {
           self.env.remove_sym(key);
       }
   }
   ```

   (`Symbol`, `with_str`, `insert_sym`, `remove_sym` are all already used in
   this file / in `methods_grammar.rs:884`.)

2. **Call it at each install site**, replacing the duplicated numeric-reset
   blocks where they exist:
   - `clear_match_state` (`regex_captures.rs:7-18`): body becomes
     `self.env.insert("/".to_string(), Value::NIL); self.reset_capture_env_vars();`
     (this also fixes the failed-match leak; `clear_multi_match_state`
     inherits it).
   - `apply_single_regex_captures`: replace the numeric-reset block at
     `regex_captures.rs:102-111` with `self.reset_capture_env_vars();`
     (position unchanged: after the `"/"` install at :100, before the
     deposits at :113-129).
   - `apply_multi_regex_captures` (`regex_captures.rs:278`): add
     `self.reset_capture_env_vars();` immediately before the `"/"` install
     at :305.
   - `smart_match_inner` single-regex arm: replace the stale-numeric block
     at `smart_match.rs:889-900` with `self.reset_capture_env_vars();`.
   - `smart_match_inner` token-RHS arm: add `self.reset_capture_env_vars();`
     right after `if let Some(mut captures) = self.regex_match_with_captures(...)`
     opens at `smart_match.rs:439`, before the positional loop at :441.
   - `dispatch_match_method` (`methods_match_dispatch.rs`): add the call
     (a) before the multi-match `"/"` install at :190 and (b) right after
     `if let Some(mut captures) = captures` opens at :202.
   - `exec_subst_op` (`vm_subst_exec.rs:7`) and
     `exec_non_destructive_subst_op` (`vm_subst_exec.rs:290`): add
     `self.reset_capture_env_vars();` once near the top of each function
     (after the pattern/replacement constants are read, before any
     matching). One entry-point call covers all ~20 `"/"` install branches
     in that file, including the failure branches, matching raku's
     clear-on-failure rule.
   - `try_native_subst` (`vm_native_subst.rs:29`): same — one call at
     function entry (this path bypasses `exec_subst_op`).
   - `dispatch_package_parse` (`methods_grammar.rs`): add the call
     immediately before the named-deposit loop at :594-599.
   - Note for vm files: use `self.reset_capture_env_vars()` as-is — the
     helper is on `Interpreter`, shared by both module trees; inside the
     helper use the direct `self.env` field exactly as the existing code in
     `regex_captures.rs` does.

3. **Do NOT touch**: the grammar action snapshot/restore machinery
   (`methods_grammar.rs:875-1012`, `:1289-1483`), the mid-match deposit
   sites (`regex_eval_repeat.rs:367/:652`), `make_regex_eval_env`
   (`regex_resolve.rs:529`), `eval_subst_replacement_cased`
   (env-restored, not a leak), and `exec_get_capture_var_op` itself (the
   read path is correct once stale keys are gone).

4. **Rebuild and re-run the probes** (all previously printed `(Match)` for
   `$<x>`; all must now print `Nil`):

   ```
   timeout 30 target/debug/mutsu -e '"ab" ~~ / $<x>=[ab] /; "cd" ~~ /cd/; say $<x>.WHAT'
   timeout 30 target/debug/mutsu -e '"ab" ~~ / $<x>=[ab] /; "zz" ~~ /q/; say $<x>.WHAT'
   timeout 30 target/debug/mutsu -e '"ab" ~~ / $<x>=[ab] /; "cd".match(/cd/); say $<x>.WHAT'
   timeout 30 target/debug/mutsu -e '"ab" ~~ / $<x>=[ab] /; my $s = "cd"; $s ~~ s/c/X/; say $<x>.WHAT'
   timeout 30 target/debug/mutsu -e 'my token t { cd }; "ab" ~~ / $<x>=[ab] /; "cd" ~~ &t; say $<x>.WHAT'
   timeout 30 target/debug/mutsu -e 'grammar G { token TOP { cd } }; "ab" ~~ / $<x>=[ab] /; G.parse("cd"); say $<x>.WHAT'
   timeout 30 target/debug/mutsu -e '"ab" ~~ / $<x>=[ab] /; "cd" ~~ m:g/cd/; say $<x>.WHAT'
   ```

   If any still leaks, `grep -rn 'insert(format!("<' src/` — every deposit
   site is enumerable by that pattern — and add the helper call before that
   site's deposit loop.

5. **Add the regression test** `t/regex-stale-named-capture-cleared.t`
   (Write tool):

   ```raku
   use v6;
   use Test;
   plan 14;

   # Before any match has ever run.
   ok $<x> === Nil, 'named capture var is Nil before any match';
   ok $0 === Nil, 'positional capture var is Nil before any match';

   "xb" ~~ / $<x>=<[cdx]> "b" /;
   is ~$<x>, 'x', 'named capture set by first match';

   "bb" ~~ / "b" "b" /;
   ok $<x> === Nil, 'name absent from current pattern reads Nil, not stale value';
   is $/.hash.elems, 0, '$/.hash is empty after captureless match';

   # A FAILED match clears $/ and named captures (measured rakudo 6.d rule).
   "xb" ~~ / $<x>=<[cdx]> "b" /;
   "zz" ~~ / "q" /;
   ok $<x> === Nil, 'failed match leaves no stale named capture';
   ok $/ === Nil, 'failed match sets $/ to Nil';

   # Positional analog stays correct.
   "ab" ~~ / a (b) /;
   "cd" ~~ / cd /;
   ok $0 === Nil, 'positional capture cleared by captureless match';

   # Sibling match paths.
   "xb" ~~ / $<x>=<[cdx]> "b" /;
   "cd".match(/cd/);
   ok $<x> === Nil, '.match clears stale named captures';

   "xb" ~~ / $<x>=<[cdx]> "b" /;
   my $s = "cd";
   $s ~~ s/c/X/;
   ok $<x> === Nil, 's/// clears stale named captures';

   my token t { cd }
   "xb" ~~ / $<x>=<[cdx]> "b" /;
   "cd" ~~ &t;
   ok $<x> === Nil, 'token smartmatch clears stale named captures';

   grammar StaleG { token TOP { cd } }
   "xb" ~~ / $<x>=<[cdx]> "b" /;
   StaleG.parse("cd");
   ok $<x> === Nil, 'Grammar.parse clears stale named captures';

   # The ticket's sub-call repro.
   sub sm1 { "xb" ~~ / $<x>=<[cdx]> "b" / }
   sub sm2 { "bb" ~~ / "b" "b" / }
   sm1();
   sm2();
   ok $<x> === Nil, 'stale named capture cleared across sub-call matches';

   # :g list-$/ — raku gives a Failure here, mutsu gives Nil after the fix;
   # !.defined is true for both, and the load-bearing part is "not the
   # stale Match".
   "xb" ~~ / $<x>=<[cdx]> "b" /;
   "cd" ~~ m:g/cd/;
   ok !$<x>.defined, 'stale named capture not visible after :g match';
   ```

   Verify with `timeout 30 target/debug/mutsu t/regex-stale-named-capture-cleared.t`
   (14/14), and sanity-check the same program under `raku` (all pass there
   today except that raku's per-routine `$/` makes the "before any match"
   asserts trivially true).

6. **Targeted regressions** before pushing (then let CI run the full
   roast):
   - `prove -e target/debug/mutsu t/capture-var-topic-slot.t` — pins the
     remove-vs-Nil distinction; if this fails, the purge inserted Nil
     instead of removing, or purged inside the action machinery.
   - `prove -e target/debug/mutsu t/match-*.t t/regex-*.t t/subst-*.t t/gram*.t`
     plus the ~56 `t/*.t` files matching `\$<` (grep list them).
   - Whitelisted roast spot-checks (`MUTSU_FUDGE=1 prove -e
     'target/release/mutsu' <file>`): `roast/S05-capture/named.t`,
     `alias.t`, `caps.t`, `subrule.t`, `match-object.t` (+ the `6.d` copy),
     `roast/S05-match/blocks.t`, `capturing-contexts.t`, `arrayhash.t`,
     `basics.t`, `make.t`, `roast/S05-grammar/action-stubs.t`, `example.t`,
     `methods.t` (6.c), `roast/S05-modifier/global.t`, `counted-match.t`,
     `pos.t`, `continue.t`, `repetition.t`, and `roast/S05-metasyntax/regex.t`
     if whitelisted. These are the heaviest consumers of `$<...>` /
     `$/`-install ordering.
   - `cargo fmt` + `cargo clippy -- -D warnings`, then `make test`.

### Known residues (acceptable, do not chase in this fix)

- `m:g` + absent name: raku returns a `Failure` (AT-KEY on a list `$/`);
  fixed mutsu returns `Nil`. Closer than the stale Match; exact Failure
  semantics belong to a Match/List AT-KEY ticket.
- A code block inside the *second* match (`/"b" { say $<x> } "b"/`) still
  sees the previous match's `<x>` mid-match, because the purge runs at
  install/clear time (match end). raku shows `Nil` there (fresh `$/`
  during the match). Same-family but needs the mid-match eval-env story.
- `$/` is per-routine lexical in raku, a single dynamic env slot in mutsu
  (`sub f { say $/.WHAT }` after a file-scope match: raku `Nil`, mutsu
  `(Match)`). Pre-existing divergence, orthogonal to this fix; worth its
  own `todo/deep/` file if it ever bites a roast test.
