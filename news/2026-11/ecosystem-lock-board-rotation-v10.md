# Ecosystem lock board rotated to #17702

The ecosystem lock board #17269 reached 255 comments, past the ~250 rotation
threshold, so it was rotated to #17702 with the `ecosystem-lock-board-rotation`
skill. The live set was computed with `live-locks.py` from the saved log.

Carried (original timestamps kept): Red `perf/17552-red-class-body-statics`,
MCP::Server::Tool::Web `fix/mcp-web-extract`, EEL
`fix/eel-hash-param-bool-return` and AttrX::Mooish `eco/attrx-mooish-090-o33`.

Not carried, released on #17269 as stale (over 24 hours old, no branch on
origin): Pod::To::HTML `eco/pod-to-html-w31`, LIVR `eco/livr-recheck-w48`, EEL
`feat/16888-eel-method-declarator` and Red `eco/red-o5`. The skills, docs,
`PLAN.md` and `.github/scripts/sync-working-label.sh` now point at #17702.
