# The compiled regex engine runs lookahead and lookbehind

The second part of ADR-0135's Slice B (#10252) compiles `<?before …>`, `<!before …>`,
`<?after …>` and `<!after …>`. A lookaround is one op that calls the walk's own lookaround test.
That test matches the body through the same entry point every other caller uses, and the entry
point answers from the body's own compiled program. So the pattern compiles only when its
lookaround bodies do. Otherwise it declines with the new reason `lookaround-body`, and the body
never drops back to the walk halfway through a compiled match.

A lookaround body therefore runs as a nested run of the compiled engine. The engine's per-run
scratch buffers are now a small pool, so a nested run reuses its own buffers instead of
allocating new ones every time.

Across all of `t/` and the roast whitelist (`scripts/rx-decline-survey.sh`), declined patterns
went from 1,792 to 1,577 and compiled ones from 5,537 to 5,679. The `lookaround` reason is gone.
20 patterns remain whose lookaround body does not compile yet.
