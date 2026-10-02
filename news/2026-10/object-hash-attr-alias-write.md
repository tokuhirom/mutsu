# Writes through a `$` alias of an object-hash attribute key by `.WHICH`

`my $t := $a.u; $t<o> = 1` on a `has %.u{Str:D}` attribute stored the key raw, because the
subscript store only recognised an object hash by a `%`-sigil variable's declared constraint.
The raw entry sat beside the `.WHICH`-keyed one the accessor write produces, so a later
`$a.u<o> = 7` went missing. The store now takes the key type from the hash the alias holds
(#10803). The generic computed-target hash store likewise keys an object hash by `.WHICH`.
