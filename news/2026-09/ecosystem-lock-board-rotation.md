# Ecosystem lock board rotated: #7884 -> #8977

The `ecosystem:lock` board — the append-only issue-comment log that lets parallel agents claim a
distribution before working it (`ecosystem-dist-roulette`, `docs/issue-workflow.md`) — grew to 318
comments (~73KB) on [#7884](https://github.com/tokuhirom/mutsu/issues/7884). A plain `get_comments`
call for it started exceeding a single tool response's size limit, so the board was no longer
reliably readable by the normal path agents use before locking.

Rotated to [#8977](https://github.com/tokuhirom/mutsu/issues/8977): the 6 locks still live at
rotation time (`Math::Interval`, `Protocol::Postgres`, `Display::Listings`, `Test::Declare`,
`WAT--CLI`, `Attribute::Lazy`) were carried forward as fresh `Locking:` comments, the `ecosystem:lock`
label moved from #7884 to #8977, and #7884 was closed with a redirect comment. Every skill and doc
that hardcoded `#7884` (`ecosystem-dist-roulette`, `ecosystem-dist-fix`, `ecosystem/README.md`,
`docs/ecosystem-parity.md`, `docs/issue-workflow.md`, `PLAN.md`, `AGENTS.md`,
`.github/scripts/sync-working-label.sh`) now points at #8977.

The new board issue documents the rotation procedure itself (a "Rotation" section) so the next
rotation — whenever the log grows unreadable again — doesn't need to be reinvented.
