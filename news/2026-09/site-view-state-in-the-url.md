# The bench dashboard and the ecosystem table put their view state in the URL

Both of the site's stateful pages had the same defect: everything the reader
chose lived only in JavaScript variables. On `bench-trend.html` the metric
(seconds / ratio vs raku / instructions), the commit window and the
charts-vs-table view could be switched freely, but the URL never moved — so
"look at the instruction counts over the last 50 commits" could not be sent to
anyone, and a reload silently dropped the selection and served the default page
back. `ecosystem.html` was the same for its search box and status filter, which
is worse there: the natural way to answer "does my module work?" is to send
someone the filtered table, and the natural way to answer "what still does not
load?" is a link to `status=blocked_load`.

Both pages now record their state in `location.hash`:

- `bench-trend.html#metric=instr&window=50&view=table&sort=jit&dir=desc`
- `ecosystem.html#q=JSON&status=partial`

Two properties make the URL worth trusting rather than merely present. Defaults
are omitted, so an untouched page keeps a bare URL and a link names only what
was actually chosen — clearing a filter takes it back out of the hash instead
of pinning `q=` forever. And the URL never describes a page that is not on
screen: a value the page cannot honour is ignored *and* rewritten out of the
hash. A link to `#metric=instr` opened against a history with no deterministic
series falls back to seconds and drops the fragment; a `#status=` naming a
status this corpus does not contain does the same. The alternative — leaving
the fragment in place — produces a URL that claims a filter is applied while
the full table is on screen, which is worse than having no state in the URL at
all.

Writes go through `history.replaceState` rather than assigning to
`location.hash`, because clicking through four metrics is one page visit, not
four history entries to back out of; typing a six-letter query into the
ecosystem search box would otherwise push six. `hashchange` is still handled,
so a hand-edited URL and a step across a real history entry both re-render, and
on the ecosystem page the status filter is re-applied after each chrome repaint
(a language switch rebuilds the `<select>`, which drops its selection).

`site/e2e.test.mjs` covers all of it: that each control lands in the hash, that
a control back at its default drops out again, that reopening such a link
restores the view, the metric, the sort, the search box and the status filter,
and that an impossible metric and an unknown status are both normalized away.
`site/README.md` gained a section listing the hash format of every page that
has one, including the two that already used the mechanism (`tutorial.html`'s
lesson key and `playground.html`'s `#code=`).
