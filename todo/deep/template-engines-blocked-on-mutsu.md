# Every Raku template engine is blocked on mutsu bugs

First measured 2026-07-25 while surveying candidates for the template battery
slot (the full table, criteria and rejections live in
[docs/batteries/templates.md](../../docs/batteries/templates.md)). The finding is
not "which engine is best" — it is that **the whole field is healthy under raku
and broken under mutsu**, so the battery decision is blocked on interpreter work.

**Re-measured in full on 2026-09-06** — every row below is a fresh count from
that run (debug build; a row counts whole upstream test files that pass), not a
carried-over figure. Several rows had gone badly stale.

| Candidate | raku | mutsu | First failure under mutsu |
| --- | --- | --- | --- |
| `Template::Mustache` 1.2.6 | 11/13 ¹ | **13/13** | none — the bundled battery |
| `Template6` 0.16.0 | 12/12 | ~~0/12~~ → **10/12** | **REDUCED AND LARGELY FIXED 2026-09-06** (`news/2026-09/template6-zero-to-ten-of-twelve.md`). The `Use of Nil in string context` headline was, as this file predicted, a pointer and not the diagnosis: reducing `Parser.compile` by deletion found four unrelated general bugs — a split-`:v` separator `Match` carried no named captures; `.subst(…, :nth(2..*))` aborted the process with a Rust `capacity overflow`; an attribute default's closure was stamped with the *constructing* class and lost its own file's subs; and an assignment to `$_` inside a nested block was discarded on block exit. Two files remain, both filed: `todo/tickets/array-arg-mutation-lost-on-the-second-call-through-a-slurpy-relay.md` (`02-for`) and `todo/tickets/template6-include-local-data-not-reaching-the-included-stash.md` (`05-includes`) |
| `Template::Jinja2` 0.2.0 | 22/23 | ~~0/23~~ → **3/23** | The two 2026-08-19 blockers are genuinely fixed, and so is the *third* one found on 2026-09-06: `lib/Template/Jinja2/Renderer.rakumod:114`'s `when If {` was parsed as a call (`Function 'If' needs parens to avoid gobbling block`) because a `use`d module's `is export`ed classes never reached the parser's type index — a trait on a declarator wraps it in a bare `Stmt::Block` the module scan did not walk into. The dist now loads; the remaining 20 files are ordinary per-feature failures |
| `Template::Mojo` 0.2.2 | 5/5 | **4/5** | `00-basic` only; residue in `todo/tickets/template-mojo-residual-failures.md` |
| `Template::Nest::Fast` 0.3.0 | 10/10 | **0/10** | `with $f ~~ m:g/…/ -> @m` binds `@m` to a one-element list *containing* the match list instead of to the list itself, so `$m[0].from` is Nil and `!index-template` warns and then dies. Reduced: `with ("a<!--x-->b<!--yy-->c" ~~ m:g/('<!--') \s* (\w+) \s* ('-->')/) -> @m { say @m.elems }` — raku 2, mutsu 1 |
| `Template::HAML` 0.9.5 | 82/83 | **39/83** ² | many; also **2–3× slower to load than raku** (release) → `todo/tickets/grammar-heavy-module-load-slower-than-raku.md` |
| `SP6` 0.2.1 | 10/11 | **10/11** | at parity — its one failure is `00-meta`, the same file raku fails |
| `Template::Classic` 0.0.3 | 1/1 | **0/1** | `Unterminated <%` thrown by its own grammar: `my grammar Grammar { token TOP { ^ [ $<part> = <text> || $<part> = <code> || <!before $> { die … } ] … } }`. The `$<part> = <rule>` capture-assignment form inside a `||` chain does not match. (The older "`Unknown method value dispatch`" note is stale.) |

¹ raku's two `91/92-specs` failures are a harness gap — they need `JSON::Fast`
from the ecosystem, which the baseline install does not have; mutsu provides it
natively, which is the only reason mutsu scores higher on that row.
² Up from 14/83 at the 2026-07-25 measurement, entirely from unrelated fixes
landed since. This row is slow to re-count (83 files under a debug build) and was
taken on the build immediately before the 2026-09-06 fixes, so it may now be
higher.

Reproduce with `tmp/tmpl-survey.sh` (fetches each dist from the REA archive at a
pinned version and runs its own suite; swap `MUTSU_BIN=raku` for the baseline).
`tmp/` is gitignored, so that script does not survive a fresh checkout — rebuild
it from the two steps it automates:

1. Look the dist's `source-url` up in the local REA index, the same data `mzef`
   uses: `~/.zef/store/rea/rea.json` is a JSON array of entries with `name`,
   `version` and `source-url` (a `https://raw.githubusercontent.com/raku/REA/…`
   tarball, URL-escaped `Name:ver<X>:auth<Y>.tar.gz`).
2. `curl -sSL` it, untar into `tmp/`, and run each `t/*.rakutest` with
   `-I lib` under `target/debug/mutsu` and under `raku`, counting whole files
   that exit 0.

## Why this is a deep item, not a ticket

It is a cluster, and the individual root causes are not yet known — each row
needs its own reduction before it can be scheduled. What *is* known:

- The `Use of Nil in string context` line is a **warning in both
  implementations** and is not itself fatal (verified with a direct repro). It is
  the first non-TAP line the harness captured, so it is a pointer into the
  failing region, **not** the diagnosis. Do not "fix the warning".
- `Template::Mojo`: the "named capture resolved as a method call" diagnosis was
  **wrong** (2026-07-26). `.characters` never was the bug: the grammar's
  `<!before '<%' ...>` / `<!before '%>' ...>` assertions failed to PARSE, because
  a quoted `<`/`>` was counted toward the angle-bracket nesting depth, so the
  `characters` token was never registered and `$<characters>` fell back to a
  method call. Fixed in #5468. **Lesson for the remaining rows: the first error
  message is a symptom; reduce the real module by deleting constructs until a
  two-line repro falls out, rather than theorising from the message.**
- `Template::Jinja2` was the cheapest lever by file count. Its 2026-07-26
  "load blocker FIXED (#5466)" note was too optimistic, and so was the
  2026-08-19 one: both blockers named there are now genuinely fixed
  (`news/2026-08/regex-char-class-literal-brace-paren.md`,
  `news/2026-08/private-method-qualified-short-owner-in-module.md`) and
  `01-lexer` passes — but a *third* load blocker was hiding behind them
  (imported `is export`ed types missing from the parse-time type index), fixed
  2026-09-06. That is three times running that a "the last Jinja2 blocker" note
  turned out to be one of several stacked at the same call site: **do not
  declare a load blocker the last one until the dist actually loads.**
- `Template6` was reduced on 2026-09-06 by deleting constructs out of
  `Parser.compile` until each divergence fell out. Four bugs, none of them the
  warning in the headline. The method worked exactly as this file prescribed
  it — write it down again for the next row.

## Already reduced and split out

- ~~`todo/tickets/q-heredoc-interpolates-qq-escape.md`~~ — **fixed 2026-07-26**
  (`news/2026-07/q-heredoc-no-escapes.md`): `Q:to/…/` wrongly honoured `\qq[…]`
  where raku leaves it literal. Found while reducing `Template6`, but it was
  **not** the `Template6` blocker — that module's failing heredocs are `q:to`,
  which mutsu handles correctly. Only the heredoc form was affected; `Q[…]` /
  `Q{…}` were already right.

## Order of work

1. ~~`Template::Mustache`~~ — **done 2026-07-25**; it is the chosen engine for the
   slot and passes 13/13. Tracked with the battery itself, not here.
2. ~~`Template::Jinja2`'s load blockers~~ — **all three done** (the 2026-08-19
   pair, plus the imported-`is export`-type parse bug found and fixed
   2026-09-06). The dist loads and runs 3/23; what is left is ordinary
   per-feature compatibility work, no longer a single lever.
3. ~~`Template6`~~ — **10/12 as of 2026-09-06**; the two residual files are filed
   as tickets and are ordinary compatibility work now, not a survey blocker.
4. `Template::Nest::Fast` — 0/10 behind a single reduced bug (`with EXPR -> @m`
   wrapping the list); likely the cheapest whole row left.
5. The rest (`Mojo`, `HAML`, `Classic`) as ordinary compatibility work; each is
   also a data point that mutsu's grammar/list semantics still diverge in ways
   ordinary modules hit.

Re-run `tmp/tmpl-survey.sh` and update
[docs/batteries/templates.md](../../docs/batteries/templates.md) after each fix;
the table is the decision input and goes stale the moment one lands.

## Measurement log

- **2026-09-01 (TRIAGE regeneration, no tarballs fetched):** confirmed from the
  news archive that both `Template::Jinja2` blockers were closed, and flagged
  the whole table as unmeasured since.
- **2026-09-06 (full re-run, all eight dists fetched):** the table above. The
  Jinja2 row had indeed moved (0/23 → 1/23, and onto a new blocker, itself
  fixed the same day to reach 3/23), `SP6` had
  silently reached parity (6/11 → 10/11), `Template::HAML` had nearly tripled
  (14/83 → 39/83) and `Template::Classic`'s recorded symptom was stale — all
  from unrelated work, none of it noticed because nobody re-ran the survey. The
  standing instruction to re-measure before quoting a row is not ceremony.
