# `$*OUT := $handle` in a mainline block redirects `say`

`{ my $*OUT = $*ERR; $*OUT := $handle; say "x" }` wrote to the old handle: the rebind of a
declared dynamic to a plain value wrote through the cell left by the earlier assignment and
never reached the `$*OUT` twin key that `say`/`put`/`print` read. A rebind of a dynamic the
code declared itself now detaches the binding and updates both spellings. Closes #16173.
