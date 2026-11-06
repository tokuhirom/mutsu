# A BagHash subclass keeps object keys through `is Subclass = :{ ... }` and `AT-KEY` overrides

`my %a is ABH = :{ $obj => 42 }` stored the key as its `.WHICH` string, `QuantHash.STORE`
treated a Hash argument as one element instead of a list of its pairs, and the internal
`__mutsu_container_at_key` read declined object keys, so an `AT-KEY` override built on
`nextcallee` died with an internal method name. All three now keep the key objects, and the
whole EERPG suite (#16867) matches `raku`.
