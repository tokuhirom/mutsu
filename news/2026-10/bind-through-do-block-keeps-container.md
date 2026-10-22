# `my $x := do { ...; %h<k> }` and `my $x := (%h<k>)` bind the element's container

A scalar `:=` whose source is a bare element (`my $x := %h<k>`) aliases the element, but the same
element written as the tail of a `do` block or inside parentheses compiled as a plain value read, so
the bound scalar was immutable (`$x = 8` died with "Cannot assign to an immutable value"). A
parenthesized source and the tail of a `do { }`, `do if`, `do with` and `do given` source now compile
in container mode, the way an `is raw` routine's tail does (`rw_tail_branch`, `compile_return_rw_arg`).
PDF's `PDF::IO::Serializer.freeze` binds `my $node-value := do with $stream { ...; $node.value<dict> }
else { ...; $node.value }` and assigns through it; its `t/io-serialize.t` is now 17/17 like raku (#14618).
