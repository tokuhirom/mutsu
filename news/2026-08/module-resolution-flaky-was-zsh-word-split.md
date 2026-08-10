# "Module resolution flaky with multiple -I paths" closed: the flakiness was zsh word-splitting in the diagnosis harness, not a mutsu bug

The ticket
`module-resolution-flaky-with-multiple-I-paths-and-global-precomp-cache`
reported that `use Cro::Policy::Timeout` intermittently failed to resolve
with identical `-I` flag sets, seemingly tracking `~/.cache/mutsu/precomp`
state.

Root cause (established over two sessions, closed 2026-08-11): the
"same flags, different outcome" observations came from running
`$BIN -I $HTTP -I $CORE ...` (and `$BIN $INC ...` with a multi-word
`$INC`) under **zsh**, which does NOT word-split unquoted variable
expansions by default. Depending on how each probe assembled its command,
some invocations passed a single mangled argument (module genuinely not
findable — "fails"), others were run through `bash -c` or with literal
paths ("passes"). The precomp-cache correlation was coincidental
sequencing of those two harness styles. A background agent's ~85 attempts
plus in-session probes (2026-08-10, see the 92nd-session memory) all
reduced to this; the ticket's own evidence section already noted that
every "this flag/count breaks it" hypothesis was falsified.

Final verification (2026-08-11, per the handoff's close-out protocol):
15 consecutive `bash -c` runs of the exact 4-flag evidence pattern
(10 with the existing precomp cache, 5 after `rm -rf
~/.cache/mutsu/precomp`) — 15/15 `ok`, zero failures, on current `main`
(release build).

Lesson (already in CLAUDE.md-adjacent memory, restated here because this
ticket is its second casualty): in this workspace's default zsh, always
wrap multi-word `-I` lists as `bash -c "..."` or use `${=VAR}`; a
"module not found" from an unquoted `$INC` expansion is a harness bug
until proven otherwise.
