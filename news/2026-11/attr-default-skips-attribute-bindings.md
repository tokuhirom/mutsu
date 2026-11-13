# An attribute default that names no attribute skips their bindings

An attribute default that is not a literal runs as a compiled chunk with
`$!x`/`$.x` bound for every attribute of the object under construction. That
is O(attributes) per default, so a class with many typed attributes paid
O(attributes²) per `.new`. Its parser-made type-object defaults (`has Str
$.s;`) and constructor-style defaults (`has $.l = Lock.new;`) are evaluated
per construction too. LLM::Chat's `Response` (29 attributes) spent ~40% of
its construction setting up and tearing down those bindings.

The compiler now scans each attribute default
(`compiler::decl_plan::attr_ref_scan`, a whitelist visitor). A default made
only of literals, barewords, calls, method calls, operators and list/hash
literals that names no `!`/`.`-twigil variable is marked attribute-free, and
runs without the bindings. A closure, an interpolating string, a regex,
`EVAL` or an indirect lookup keeps them. `Response.new` went from 405 µs to
297 µs.
