# Subscript stores through an argument-less rw method land in its location

`class D { has %.h; method el() is rw { %!h<a> } }` hands back the location of
`%!h<a>`. With `$d.el[0]++`, `$d.el()[0] = 5` or `$d.el<k> = 3` on a missing
key, mutsu dropped the write and left `%.h` empty; on an existing key it
replaced the element's itemized container with a non-itemized copy
(`{:a({:k(2)})}` instead of `{:a(${:k(2)})}`). The accessor-writeback builtin
now stores through the location the method returns: a container it holds is
written into in place, and an empty one gets the Array or Hash the subscript
addresses vivified into it, as rakudo does. (#11355)

Two neighbouring gaps — an rw `$.x` attribute holding `Any` not vivifying,
and a location holding a plain value not refusing the store — are filed as
#11653.
