# MAIN usage messages follow Rakudo's format

The generated `MAIN` usage message (`$*USAGE`, and what a failed dispatch
prints) is now a port of Rakudo's `default-generate-usage`. Named options come
before positionals (unless `%*SUB-MAIN-OPTS<named-anywhere>`), an option whose
type accepts `True` shows its value as optional (`[=Int]`, `--any[=Any]`) and
any other as required (`=<Str>`), alias chains render as `-l|--length` with a
single dash for one-letter names, subset types are named by the subset, and a
routine's `#|` documentation follows its line after `--`. Parameters
documented with `#=` get a second table with their `[default: ...]`, long
defaults elided as Rakudo does.

Candidates are listed in declaration order; a failed dispatch whose first
argument selects literal sub-command candidates (`multi MAIN('add', ...)`)
shows only those, and `--help` prints to `$*OUT` with exit code 0 only when no
candidate takes it. `RUN-MAIN(&main, ...)` now dispatches to and describes the
callable it was given rather than whatever `MAIN` is in scope, so
`RUN-MAIN(&new-main, Nil)` prints `<name> [<stuff> ...]` instead of an empty
`Usage:`. A named alias chain accepts every one of its links on the command
line (`:x(:y(:$zed))` takes `--y=5`).
