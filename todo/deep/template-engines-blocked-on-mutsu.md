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
| `Template::Jinja2` 0.2.0 | 22/23 | **0/23** | **reduced** → `todo/tickets/private-method-call-inside-closure.md`. One error, 22 files |
| `Template::Mojo` 0.2.2 | 5/5 | **0/5** | **reduced** → `todo/tickets/grammar-named-capture-resolved-as-method.md` |
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
- `Template::Mojo`: `.characters` is not a raku `Match` method either — the
  module has `token characters` in its grammar and the actions read
  `$<characters>`. So mutsu is resolving a **named capture as a method call**
  somewhere the grammar subrule should have been found. The grammar is a
  file-scope `grammar` block using `||` alternation with forward-referenced
  tokens (`lib/Template/Mojo.rakumod:1-40`); a direct small-grammar repro did
  *not* reproduce it, so the trigger is narrower than "named capture in an
  action".
- `Template::Jinja2` is the **cheapest lever by file count** — one load-time
  error takes out 22 of 23 files. Start here if optimising for measured
  progress.

## Already reduced and split out

- `todo/tickets/q-heredoc-interpolates-qq-escape.md` — `Q:to/…/` wrongly honours
  `\qq[…]` (raku leaves it literal). Found while reducing `Template6`; it is a
  real bug on its own, but it is **not** the `Template6` blocker, since that
  module's failing heredocs are `q:to`, which mutsu handles correctly.

## Order of work

1. ~~`Template::Mustache`~~ — **done 2026-07-25**, and it is the chosen engine for
   the slot. One general fix (hyper Slip flattening) took it 1/13 → 11/13. The
   last two files are tracked with the battery itself, not here.
2. `Template::Jinja2`'s private-method error (1 error → 22 files) — the cheapest
   remaining lever by a wide margin.
3. `Template6` — the runner-up candidate; worth fixing so the survey has a real
   second option rather than a single viable choice.
4. The rest (`Mojo`, `Nest::Fast`, `HAML`, `SP6`, `Classic`) as ordinary
   compatibility work; each is also a data point that mutsu's grammar/list
   semantics still diverge in ways ordinary modules hit.

Re-run `tmp/tmpl-survey.sh` and update
[docs/batteries/templates.md](../../docs/batteries/templates.md) after each fix;
the table is the decision input and goes stale the moment one lands.
