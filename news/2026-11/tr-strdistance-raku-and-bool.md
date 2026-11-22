# `tr///`'s `StrDistance` renders and boolifies like Rakudo's

The `StrDistance` a destructive `tr///` returns printed as a bare
`StrDistance.new`: it has no class declaration in mutsu, so the generic
instance `.raku` found no public attributes. It now renders
`StrDistance.new(before => "hello", after => "heLLo")` for both `.raku` and
`.gist`. Its truth is now its distance's, so a transliteration that changed
nothing is `False` instead of always `True` (#18903).
