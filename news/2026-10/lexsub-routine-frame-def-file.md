# A block-declared routine's frame names the file that declares it

A routine declared in a block (`sub helper` inside a `supply`) that is called
from a compiled closure pushed its call frame with the *calling script's* file
as `def_file`, so the module's own `use` of another package stopped applying and
`Other::secret()` died with "Could not find symbol". The cause was a stale
memoized `source_file_sym`: `compiled_lazy`'s `adapt` clones the compiled body
(carrying the symbol memoized from the compile-time ambient file) and then
overwrites `source_file` without resetting that memo. It now resets it, and the
compiled-closure fork in `call_sub_value` no longer needs to keep closures that
name a block-declared routine on the tree-walk carrier (#14507).
