# `my $p := $.w` binds a copy for a non-rw public attribute

`$.w` is `self.w`: a public accessor hands back the attribute's container only
when the attribute is `is rw`. The bind tagged the `.w` local, which is a
by-name alias of the attribute, so assigning through `$p` changed a plain
(non-rw) attribute too. The bind now asks the accessor call for its container
(the same request a `:=` to a method-call RHS makes), so an `is rw` attribute
still aliases and a plain one binds an assignable copy.
