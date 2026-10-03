# A sigilless `\x` no longer leaks out of its block

The compiler tracked sigilless bindings (`my \x = ...`) in a flat set that was
never scoped, and a `$x` variable shares the `x` local key. So once any block
had declared `my \x`, every later `my $x` in the compilation unit was treated
as a sigilless bind and stored its value un-itemized:
`{ my \x = 1 }; { my $x = [1,2]; say $x.raku }` printed `[1, 2]` instead of
`$[1, 2]`. A block now restores the whole set of sigilless names on exit
(previously only names of native types were restored), so the binding ends
with its scope (#11228).
