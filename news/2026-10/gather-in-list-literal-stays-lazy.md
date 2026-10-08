# A gather inside a list literal no longer hangs

`(1, gather { for 1..* { take $_ } })` used to run the gather while the list was being built, so an
infinite body hung. The list literal now keeps the gather unforced; `.raku` reifies a held gather at
render time, as Rakudo does. Array literals (`[...]`) still run a finite gather eagerly.
