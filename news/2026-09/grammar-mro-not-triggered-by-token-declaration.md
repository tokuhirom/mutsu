# A class or role declaring a token/rule no longer looks like a Grammar in `.^mro`

`.^mro` and `.^parents` used `package_looks_like_grammar`, a heuristic that classified any
package as a grammar the moment a `token`/`rule` was registered anywhere under its name. A plain
`class N { rule foo { x } }`, or a `class C is BaseG { }` where `BaseG` is a role that merely
declares a token, wrongly threaded `Grammar -> Match -> Capture -> Cool` into the class's MRO —
found while working `CSS::TagSet`, whose `CSS::Module::CSS3::Namespaces` is a `unit class` with a
`rule`.

Both call sites (the MRO computation and the native `parse`/`subparse`/`parsefile` method probe)
now use `class_is_grammar`, which walks the registry's real `is`-parent chain instead of guessing
from token registrations — the same predicate `class_dispatch.rs` and other grammar-instance
routing already relied on for the identical question. A `grammar` declaration always threads
`Grammar` into its own parent list, so every existing grammar MRO test (empty grammars,
token-less subclasses, the `Grammar` type object itself) keeps passing unchanged.
