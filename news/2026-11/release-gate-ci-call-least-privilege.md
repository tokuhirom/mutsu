# The release workflow starts again: the main-failure report leaves ci.yml

A `workflow_dispatch` smoke run of `release.yml` on 2026-11-25 ended in `startup_failure` before
any job ran. `release.yml` calls `ci.yml` as the gate in front of every publish and grants that
call read-only permissions. A reusable workflow may not ask for more than its caller grants.
`ci.yml` had gained a `report` job, which opens the `ci:main` tracking issue when a scheduled run
fails, and that job asked for `issues: write`. The last release workflow run, on v0.26.1
(2026-11-01), predates the job, so the next real `v*` tag would have failed the same way.

The report now lives in its own workflow, `ci-main-report.yml`. It runs on `workflow_run` after a
scheduled `CI` run fails, reads the failed job names from the run's jobs API, and opens or updates
the issue as before. `ci.yml` is read-only again, and the release gate keeps its least-privilege
grant.
