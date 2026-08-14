# Template::Mojo triage closed out — all findings resolved or redirected

`todo/tickets/template-mojo-residual-failures.md` tracked three independent
findings surfaced by running `Template::Mojo` 0.2.2's own test suite against
mutsu after the original quoted-angle-bracket regex parse fix
(`news/2026-07/regex-assertion-quoted-angle-brackets.md`). All three are now
dispositioned, so the ticket is closed with nothing left independently
actionable in it:

1. **`00-basic` test 16** (EVAL'd named-sub arity error message/phrasing) —
   fixed, `news/2026-08/eval-named-sub-value-call-arity-message.md`.
2. **`00-basic` test 17** ("too many arguments" for a placeholder-arity sub
   called through EVAL) — investigated and deliberately left failing: the
   legacy binder has no reliable way to distinguish a `^`-twigil placeholder
   sub with exact arity from one that also reads `@_`/`%_` (which legitimately
   accepts extra positionals), and the only fix attempted leaked a synthetic
   `params` shape into ~80 other call sites that read a Sub's params list
   verbatim. A real fix needs a dedicated field threaded from the AST through
   to the runtime `Sub` value — recorded in the (now-deleted) ticket's history
   for whoever picks this back up, not worth a fresh `todo/` file since
   there is no live regression, just an unimplemented edge case.
3. **`03-capture`** — two layers: the Nil-vs-empty-Match capture bug is fixed
   (`news/2026-08/regex-token-named-optional-atom-empty-match-not-nil.md`);
   the remaining blocker (`rule`/`:sigspace` not consuming whitespace trailing
   a rule's last atom) is a general grammar-engine gap, already tracked on
   its own in `todo/tickets/rule-sigspace-does-not-consume-trailing-whitespace.md`
   (which already cross-references this dist as the motivating case).

No new code changes in this entry — it is a bookkeeping close-out of a
now-fully-triaged ticket.
