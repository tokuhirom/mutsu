# `.VAR.name` of a `:=` alias of an element reports the owner

`my $y := @a[1]; $y.VAR.name` said `$y` where rakudo says `@a` (and `%k` for a
hash element), and `dd $y` printed `$y = 2` instead of `element of @a = 2`. The
element's cell is now named after its owning array or hash when it is promoted
for a bind, ahead of the bind naming it after the alias; `dd` prefixes such a
name with `element of`.
