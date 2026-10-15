# Tagged proto methods export callable subs

`proto method ... is export(:tag)` now installs the method's forwarding sub in that export tag. The forwarding sub passes its capture to the method proto, preserving positional and named arguments. This fixes the import shape used by PDF::Font::Loader's `load-font` declaration.
