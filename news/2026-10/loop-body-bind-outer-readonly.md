# A `my $x := value` in a loop body no longer makes an enclosing `$x` readonly

The readonly registry is keyed by bare name, so `for ... { my $purl := f($_) }`
marked `purl` readonly for good, and a later `$purl = ...` on an enclosing
`my $purl` died with `Cannot assign to an immutable value`. A loop (or
branch) body now drops the marks of the names it declared when its scope
ends, as a bare block already did. The `PURL` distribution's
`t/01-basic.rakutest` now matches rakudo (111/111).
