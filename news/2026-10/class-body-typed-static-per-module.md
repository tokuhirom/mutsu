# Two modules' same-named typed class statics keep their own types

A `unit class` body runs in its loader's env, and the declared element and
key types of its `@`/`%` statics (`my %seen{List}`, `my Int %counts`) lived
in name-keyed env lanes that outlived the body. So the next module declaring
a same-named static (`my %seen{Hash}`) overwrote them, and the first class's
methods checked their stores against the second class's types (PDF's
`PDF::COS::Array` / `PDF::COS::Dict`, #12903). Leaving a class body now
records those lanes per class and restores the enclosing scope's, and method
entry injects them with the class's statics, as #10524 already did for typed
scalar statics.

Found on the way: the compiled method fast binder bound a positional
argument to a plain `$` parameter without itemizing it, so
`method m($k) { %h{$k} = 1 }` called with a List sliced the hash instead of
storing one key. It now binds as the sub binders do.
