# A version-literal-shaped bareword before `=>` is a pair key

`v33 => False` (and `v1 => 5`, `v6 => 2`) was parsed as the version literal `v33` on the
left of the fat arrow, so `Foo.new(v33 => False)` never bound the `$.v33` attribute. The
version-literal parser now declines when the token is followed by `=>`, leaving it to the
bareword-pair-key path. Found with RPi::Device::ST7036, whose `t/00-load.rakutest` now
passes like rakudo.
