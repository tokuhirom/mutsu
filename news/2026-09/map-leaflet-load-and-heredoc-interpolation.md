# Map::Leaflet loads cleanly and keeps imported heredoc calls

`Map::Leaflet` 0.0.12 could not load under mutsu because two array
destructuring parameters in one signature were both represented by the
anonymous name `@`. The duplicate-parameter check now treats anonymous
sigils like the other compiler-generated parameter names.

The distribution also exposed a separate cold-start bug in heredoc
interpolation. A call to an imported routine inside `{ ... }` was parsed as a
statement call; expression compilation then discarded its return value and
interpolated `Nil`. Statement calls in expression position now use the same
value-producing compilation path as other tail calls.

`Map::Leaflet` moves from `blocked_load` to `green`: all 6 files load and
match rakudo, with 33/33 assertions passing under the release-binary
ecosystem sweep.
