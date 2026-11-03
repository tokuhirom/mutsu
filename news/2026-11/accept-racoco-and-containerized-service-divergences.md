# Two environment divergences recorded as accepted

The maintainer decided on 2026-11-03 that mutsu does not ship a `raku`-named executable beside
`mutsu` (#15807), and that Dev::ContainerizedService's `t/clickhouse.t` is an accepted divergence
(#15525): the oracle rakudo has no Cro and skips, while mutsu bundles `Cro::HTTP::Client`, runs
the real flow and needs a Docker daemon.

`ecosystem/accepted-divergences.toml` now pins the four test files (three of App::RaCoCo, one of
Dev::ContainerizedService) to their exact failure shapes, so they leave the KPI denominator and
the "what to fix next" tools stop offering them, while any other way of failing re-grades them.
