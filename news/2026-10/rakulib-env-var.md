# `RAKULIB` is read as a module search location

mutsu now reads the `RAKULIB` environment variable, in Rakudo's format: a
comma-separated list of `inst#PATH`, `file#PATH` or bare-path repository specs
(entries with any other `scheme#` prefix are ignored). It is searched right
after `MUTSULIB`, so the order is `use lib`, `-I`, `MUTSULIB`, `RAKULIB`,
installed, bundled; like `MUTSULIB` it stays out of `%*COMPILING<%?OPTIONS><I>`.

Pakku starts a staged distribution's test and build children as `$*EXECUTABLE`
with `RAKULIB=inst#<stage>,inst#<target>`; before this the child could not find
the staged distribution (mutsu#14918). The trust-boundary note in
`docs/security.md` and the search-order lines in `AGENTS.md` list the new
location.
