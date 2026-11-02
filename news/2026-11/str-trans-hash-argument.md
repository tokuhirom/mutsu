# `Str.trans` accepts a Hash

`$str.trans(%map)` died with "Only Pair objects are allowed as arguments to Str.trans, got
Hash". Rakudo's `trans` takes `*@changes`, so a Hash flattens into its pairs; mutsu now does
the same. Found with DAWG's Unicode compression (`$str.trans(%!unicode-map)`), whose
`t/04-edge-cases.rakutest` runs 17 of 20 assertions instead of 6.
