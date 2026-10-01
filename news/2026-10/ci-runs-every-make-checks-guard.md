# CI runs every `make checks` guard, and checks that it does

CI's `test-check` job runs each static guard as its own named step instead of
`make checks`. Two guards were added to `make checks` without a step:
`check-value-wall` and the AST-walker ratchet `check-ast-walkers` (ADR-0137).
Both ran only on developer machines and in `scripts/dev gate`. Within a day of
the walker ratchet landing, three PRs merged six new hand-rolled walkers past it.
After that, every local `make checks`, and so every agent's gate, failed on a
clean `main`.

Those walkers were listed in `scripts/ast-walkers-baseline.txt` separately (52fb43366).

Both guards are now CI steps. A new step derives the target list from the
Makefile's `checks:` line and fails when any target has no `run: make <target>`
step in `ci.yml`, so a guard added later cannot be left out the same way.
