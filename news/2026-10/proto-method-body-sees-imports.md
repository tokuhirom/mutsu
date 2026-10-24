# A `proto method` body sees the routines its class's file imported

A `proto method` body was compiled when the method was first called, by a compiler with none of the
enclosing scope, so a routine the file imported with `use` (`proto method fly { {*}; CATCH { default
{ log ... } } }`) died with `Unknown function`. The main pass now compiles the body in the class's
lexical context, like every other method body, and registration installs that compile. Pakku's
`Pakku.fly` error path depends on it (mutsu#14870).
