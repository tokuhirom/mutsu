# `CLIENT::` sees a module's `.map` block; `$?PACKAGE` in such a block is right

`CLIENT::` now finds the calling code by walking the call frames' caller code
and comparing the package each routine or closure body was declared in. This
package is recorded on `CompiledCode` when the body is compiled. A module
routine that returns `<a>.map: { code }` runs the block only once the `Seq` is
sunk, after the routine has returned. The block now still counts as the
module's code calling back, so `CLIENT::CORE::<CORE-SETTING-REV>` and
`CLIENT::LEXICAL::<$?PACKAGE>` report the module's revision and package
(#18845, roast `S02-names/pseudo-6e.t` tests 178, 179, 184, 185).

The map/grep inline-loop recompile of a block body now keeps the package the
block was written in. Before, `$?PACKAGE` inside a module routine's `.map`
block read `GLOBAL`.
