# Imported type smileys in grouped expressions

An imported type that was also registered as a sigilless value term could lose
its `:D`, `:U`, or `:_` smiley inside parentheses. For example,
`use NativeCall; !(1 ~~ CArray:D)` failed to parse. The term parser now lets the
identifier parser consume a type smiley as part of the type name. Ordinary
sigilless terms and longer adverbs keep their existing parsing.

This lets `Math::DistanceFunctionish`, a dependency of `ML::Clustering`, load
through the grouped `CArray:D` checks in its `args-check` method. A focused test
pins the imported type behavior.
