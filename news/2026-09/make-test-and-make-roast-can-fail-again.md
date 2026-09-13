# `make test` and `make roast` can fail again

Both suite targets ended their recipe in `| tee tmp/make-*.log`, and make runs recipes
under `/bin/sh` with no `pipefail`, so the status make saw was **`tee`'s** — which is
always 0. A failing `prove`, a failing `cargo test`, and even a failing
`cargo build --release` all left `make test` exiting 0:

```console
$ sh -c '(false) 2>&1 | tee /dev/null; echo "pipeline status=$?"'
pipeline status=0
```

That made the two commands CLAUDE.md names as the pre-publication gate —

> Before opening a PR, run `cargo fmt --all`, `make lint`, `make test` and `make roast`
> once each, and do not publish until both suites are green.

— unable to report anything but success. It was caught for real while working #8087
stage 3: one `#[test]` in `src/runtime/meta_ns.rs` failed, the log ended in
`test result: FAILED. 1134 passed; 1 failed`, and `make test` still exited 0. Only a
by-hand grep of the log noticed. The prerequisites (`check-t-layout` and friends) failed
correctly all along, being separate targets rather than part of the piped line, so
`make test` failed on a layout violation and passed on a failing test suite — precisely
the wrong way round. CI was never affected: `ci.yml` runs the steps individually and
never invokes these targets, which is exactly why the defect could sit there unnoticed.

The recipes now run under a shell that propagates it:

```make
SHELL := /bin/bash
.SHELLFLAGS := -o pipefail -c
```

Two lines fix both targets and any future piped recipe, and `tee` keeps writing the logs
as before.

## The guard

A fix to a silent-failure bug that is itself silent when reverted is worth little, so
`check-pipefail` is a new target — a prerequisite of both `test` and `roast`, and a CI
step next to the ledger checks — that fails if a false-in-the-pipeline is ever masked
again:

```make
check-pipefail:
	@if (exit 1) 2>&1 | tee /dev/null; then \
		echo 'check-pipefail: FAILED -- ...' >&2; exit 1; \
	fi
```

It builds nothing and costs milliseconds. Verified both directions: `make check-pipefail`
exits 0 as configured, and `make check-pipefail SHELL=/bin/sh .SHELLFLAGS=-c` — the
simulated revert — fails with the diagnostic. A suite-shaped recipe
(`(echo building && false) 2>&1 | tee log`) now exits non-zero while still writing its
log. CI gets the step even though CI does not use these targets, because nothing else
there ever looks at the property.

## The documentation this had bent

The masking had quietly reshaped the instructions around it. CLAUDE.md's
"Checking `make test` / `make roast` results" told agents to grep `tmp/make-test.log`
*instead of* trusting the command, which was the only workable advice while the exit
status was a constant — and it taught judging green by eye, which is both expensive and
easy to get wrong. That section, the matching bullet in "Run both full suites yourself",
the `AGENTS.md` summary, and the `mutsu-ticket-flow` skill now all draw the line the same
way: **the exit status is the verdict, the log is the detail.** Read the log to find
*which* file failed, never to find out *whether* something failed, and never by re-running
a suite to see output that is already on disk.
