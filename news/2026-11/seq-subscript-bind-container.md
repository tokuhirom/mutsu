# Binding a subscript of a mapped Seq hands out the element container

`my $x := $seq[0]` on a Seq built by `.map({ %h{$_} })` decontainerized the element, so
`$x = 9` died with "Cannot assign to an immutable value" (CustomImporting `t/01-import`).
The bind-mode (`IndexAutovivifyLazyTerminal`) subscript now reifies the needed prefix of a
Seq and returns the stored cell, as it already did for an `Array`.
