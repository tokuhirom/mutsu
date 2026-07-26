# Every Raku template engine is blocked on mutsu bugs

Measured 2026-07-25 while surveying candidates for the template battery slot
(the full table, criteria and rejections live in
[docs/batteries/templates.md](../../docs/batteries/templates.md)). The finding is
not "which engine is best" — it is that **the whole field is healthy under raku
and broken under mutsu**, so the battery decision is blocked on interpreter work.

| Candidate | raku | mutsu | First failure under mutsu |
| --- | --- | --- | --- |
| `Template::Mustache` 1.2.6 | 11/13 | ~~1/13~~ → **11/13** | **FIXED** — hyper `».method` did not flatten a `Slip` result; pin `t/hyper-method-slip-result.t`. Two files remain: `06-logging` (2/3), `92-specs-file` (1/10) |
| `Template6` 0.16.0 | 12/12 | **0/12** | `Use of Nil in string context` from `Parser.compile`: a `q:to/RAKU/` heredoc whose `\qq[$safe-delimiter]` / `\qq[$segment]` come out empty |
| `Template::Jinja2` 0.2.0 | 22/23 | **0/23 files**, but no longer dies at load | **Load blocker FIXED 2026-07-26** (#5466, `news/2026-07/private-method-in-closure.md`): a closure created in a method lost its class, so `Renderer.rakumod`'s `sub (*@args) { self!cycle(|@args) }` was rejected and every file died before its first assertion. The suite now RUNS (re-measured 2026-07-26 with the dist from the REA archive) and fails on assertions instead — e.g. `01-lexer` reaches subtest 1 and fails "Correct value". A new reduction is needed for that next layer |
| `Template::Mojo` 0.2.2 | 5/5 | ~~0/5~~ → **4 of 5 files run** | **FIXED 2026-07-26** (#5468, `news/2026-07/regex-assertion-quoted-angle-brackets.md`): a quoted `<`/`>` inside a regex lookaround broke the parse — not a named-capture bug. Now 00-basic 15/17, 01-template 3/3, 02-complex 1/1, 04-native-named 1/1; residue in `todo/tickets/template-mojo-residual-failures.md` |
| `Template::Nest::Fast` 0.3.0 | 10/10 | **0/10** | `Use of Nil in string context` |
| `Template::HAML` 0.9.5 | 82/83 | 14/83 | many; also **2–3× slower to load than raku** (release) → `todo/tickets/grammar-heavy-module-load-slower-than-raku.md` |
| `SP6` 0.2.1 | 10/11 | 6/11 | `Use of uninitialized value element of type Any in string context` |
| `Template::Classic` 0.0.3 | 1/1 | 0/1 | `X::Method::NotFound: Unknown method value dispatch (fallback dispatch)` |

Reproduce with `tmp/tmpl-survey.sh` (fetches each dist from the REA archive at a
pinned version and runs its own suite; swap `MUTSU_BIN=raku` for the baseline).

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
- `Template::Jinja2` was the cheapest lever by file count and its load-time
  error is now fixed (#5466); the suite runs but fails on assertions, so it
  needs a fresh reduction rather than a re-run.

## Already reduced and split out

- ~~`todo/tickets/q-heredoc-interpolates-qq-escape.md`~~ — **fixed 2026-07-26**
  (`news/2026-07/q-heredoc-no-escapes.md`): `Q:to/…/` wrongly honoured `\qq[…]`
  where raku leaves it literal. Found while reducing `Template6`, but it was
  **not** the `Template6` blocker — that module's failing heredocs are `q:to`,
  which mutsu handles correctly. Only the heredoc form was affected; `Q[…]` /
  `Q{…}` were already right.

## Order of work

1. ~~`Template::Mustache`~~ — **done 2026-07-25**, and it is the chosen engine for
   the slot. One general fix (hyper Slip flattening) took it 1/13 → 11/13. The
   last two files are tracked with the battery itself, not here.
2. ~~`Template::Jinja2`'s private-method error~~ — **done 2026-07-26** (#5466);
   the suite now runs. Its next layer (assertion failures, starting with
   `01-lexer`'s "Correct value") is unreduced and is the natural follow-up.
3. `Template6` — the runner-up candidate; worth fixing so the survey has a real
   second option rather than a single viable choice.
4. The rest (`Mojo`, `Nest::Fast`, `HAML`, `SP6`, `Classic`) as ordinary
   compatibility work; each is also a data point that mutsu's grammar/list
   semantics still diverge in ways ordinary modules hit.

Re-run `tmp/tmpl-survey.sh` and update
[docs/batteries/templates.md](../../docs/batteries/templates.md) after each fix;
the table is the decision input and goes stale the moment one lands.
