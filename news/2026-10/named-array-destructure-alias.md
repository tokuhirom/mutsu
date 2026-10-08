# Named array-destructuring parameters with nested named aliases

`:out([$out?, :pass($out_pass) = True])` no longer reports a false
`X::Redeclaration` for the anonymous `@` pattern wrapper, and the nested
`:pass(...)` alias now binds its default instead of the array element. The
duplicate-name check and the named-rename binder share the same view of which
sub-signature names declare variables and which are argument keys (#11917).
