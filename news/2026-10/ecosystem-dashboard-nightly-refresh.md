# The ecosystem dashboard refreshes after every nightly sweep

`ecosystem-sweep.yml` has a new `dashboard` job that runs after `collect` and rewrites the body of
the dashboard issue (#15488) with `scripts/ecosystem-dashboard.py --update`.

It is a separate job on purpose. `sweep` runs third-party test suites and holds no write token, and
`collect` pushes the ledger with `contents: write` next to their output. The dashboard job needs
only `issues: write`, reads none of the shards' output, and rewrites one issue body, so two runs
cannot lose each other's edit. The step is `continue-on-error`, so a failed refresh never turns the
sweep red.

Because the body is built partly from comments anyone can post on the lock board, the script now
keeps only lock holders whose distribution and branch match a name allowlist; anything with markup
in it is ignored.
