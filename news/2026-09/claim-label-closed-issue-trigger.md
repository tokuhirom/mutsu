# The `working` label now clears when an issue closes

`.github/workflows/claim-label.yml` derives the `working` label purely by
replaying an issue's comment log (a `Claiming: <branch>` turns it on, the
matching `Releasing: <branch>` turns it off). Closing an issue posts no
comment, and the workflow only triggered on `issue_comment`, so nothing ever
observed a close — a `Closes #NNNN` merge left the issue reading as claimed
forever, and the scheduled stale-claim sweep only looks at *open* issues.
Two real instances: #8221 (closed by #8237) and #8201 (closed by #8252), both
still carrying `working` until released by hand.

Fixed two ways:

- `claim-label.yml` now also triggers on `issues: [closed, reopened]`.
- `sync-working-label.sh` treats a closed issue as having no live claim
  regardless of what the log's last line says (`effective_live_count`, a pure
  function so the self-test can exercise it without hitting the API) — no
  synthetic `Releasing:` comment is posted, since the log stays exactly what
  happened and the closed state alone is enough to suppress the label. On
  reopen, the label re-derives from the log as before, so an outstanding claim
  comes back. `sync_all` (the scheduled/`--all` run) also sweeps already-closed
  issues that still carry the label, to clean up instances from before this
  fix existed.

See [#8256](https://github.com/tokuhirom/mutsu/issues/8256).
