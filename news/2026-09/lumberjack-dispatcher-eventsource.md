# Lumberjack::Dispatcher::EventSource now passes its full suite

`Lumberjack::Dispatcher::EventSource` now runs its metadata and basic tests
under mutsu. The fixes cover live `Supply.merge` dispatch, imported
categorical trait handlers in module scope, qualified enum coercion,
`Backtrace::Frame` construction, and persistence of multiple runtime traits on
the same attribute.

The distribution moves from `partial` (1/2 baseline files) to `green` (2/2,
7/7 assertions).
