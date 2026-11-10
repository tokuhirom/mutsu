# An exported `our` dynamic variable is read through the dynamic scope

A module's `our $*X is export` is a dynamic: its own class reads it from the
caller's dynamic scope, where the importer's `$*X := ...` binds it. mutsu
redirected the read to the declaring package's mirror cell whenever another
module had loaded the exporter first, so the class saw `Nil` and Qwiratry's
`$*TRANSFORM-ROOT` (`mold-when-query`) never reached `Mold.matches`. A dynamic
is no longer registered as a package scalar cell (#13370).
