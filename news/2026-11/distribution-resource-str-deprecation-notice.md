# `Distribution::Resource.Str` records rakudo's deprecation notice

A direct `.Str` on a `%?RESOURCES` entry now records
`Method Str (from Distribution::Resource) ... Please use %?RESOURCES<key> directly
instead.` at the call site, as rakudo does. The shared `IO::Path` `Str` row became
an interpreter row for this (the notice needs the call site's file and line), and the
method-site lane adopts the source line before running an interpreter row (#16055).
