# `pick` and `roll` take the `Whatever` type object as a count

`(^5).pick(Whatever)` died with "No such method 'pick' for invocant of type
'Range'", and `pick(Whatever, @list)` died with "Unknown function: pick".
mutsu only knew the `*` instance as a "take everything" count. In rakudo the
`Whatever` type object binds the same `Whatever` parameter, so it is a count
there too. Data::Generators relies on this: its `random-word(Whatever)`
resolves `&method` to `&pick` and calls it with the size `Whatever`.

Both methods now read the type object as `*`. The sub form of `roll` had its
own copy of the count handling, one that did not know the type object either;
it now delegates to the method, as `pick`'s sub form already did.
Data::Generators' `t/02-random-word` now passes 15/15; before, it died after
10.
