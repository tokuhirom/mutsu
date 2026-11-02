# A module-private wrapped sub no longer shadows the core routine

A private `sub open(...)` in a `use`d module that had been `.wrap`ped (which is
what `is native` does to every native routine) captured the importing script's
calls to the core `open`, because a wrapped routine is recorded by bare name and
`wrapped_sub_is_the_callee` accepted it whenever nothing else answered the name.
It now accepts it only when the routine is reachable by scope: imported,
declared in the running package, or global (#16010).
