# A dependency's class no longer makes a required module's `unit class` too late

When a run-time `require ::("Some::Module")` compiled a module on a cold
cache, the parse-time scan of that module's own `use`d dependency left the
dependency's `class` behind as if the requiring module had declared it. The
module's `unit class` then failed with X::UnitScope::TooLate ("Too late for
unit-scoped class definition"), the check #18680 added. Red's
`database "SQLite"` hit it while loading `Red::Driver::SQLite`. The module
scan now saves and restores the "a package was declared" flag around its
nested parse, as it already did for the scopes and the package path (#18767).
