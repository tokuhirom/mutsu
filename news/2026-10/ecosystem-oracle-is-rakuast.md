# The ecosystem oracle is rakudo's RakuAST frontend

The ecosystem baseline is now explicitly rakudo's default RakuAST frontend.
A distribution that does not work on rakudo under RakuAST does not have to
work on mutsu, and its files are `no_baseline` (maintainer decision,
2026-10-21). The motivating case is Test::Async. Its slang actions build QAST
and call `$*W`, it passes 27 of 28 files only under `RAKUDO_RAKUAST=0`, and it
fails on rakudo's RakuAST frontend. Such modules target a compiler surface that
is no longer maintained.

`scripts/ecosystem-sweep.py` now sets `RAKUDO_RAKUAST=1` for every run, so a
caller's environment cannot switch the oracle to the legacy frontend. mutsu
ignores the variable. The rule is in AGENTS.md, `docs/ecosystem-parity.md`
and the `ecosystem-dist-fix` skill.
