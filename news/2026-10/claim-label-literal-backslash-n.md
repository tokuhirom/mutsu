# The claim-log parser ignores a literal `\n` typed after a branch name

A `Claiming:` comment whose agent typed the two characters `\n` instead of a line break
(`fix/14512-rakuast-regex-arguments\n\nTaking ...`) named a branch that never exists on `origin`, so
`claim-label.yml` auto-released it as stale every few hours. A git branch name cannot contain a backslash,
so `sync-working-label.sh` now cuts the branch field at the first backslash, for `Claiming:` and
`Releasing:` alike. The self-test pins both directions.
