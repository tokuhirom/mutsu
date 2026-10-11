# A sigilless `\r` parameter no longer reads an outer shadowed binding

When an outer scope had a sigilless `my \r` shadowed by a same-named `my $r`, a
nested routine's own `\r` parameter (and a `my $r` inside it) kept reading the outer
parked value instead of the parameter. The compiler now treats a sigilless
binding declared after the shadow as owning the bare name again, and re-shadows it
when a scalar of the same spelling is declared. `Method::Also`'s
`method specialize(Mu \r, ...)` followed by `my $r := callsame` is the real-world shape
(#12637).
