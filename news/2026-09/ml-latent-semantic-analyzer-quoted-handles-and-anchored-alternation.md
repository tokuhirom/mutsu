# ML::LatentSemanticAnalyzer reaches parity

The `ML::LatentSemanticAnalyzer` 0.0.3 load probe failed because mutsu
misparsed a quoted `handles` rename such as `"nrow" => "nrow"`, treating the
attribute declaration as a call and losing its invocant. Quoted exposed names
now support the same `=>` rename form as bare names.

Its CSV resource parser also exposed a regex bug: anchors inside top-level
alternation branches were ignored during nested matching, so
`/^ '"' | '"' $/` removed every quote instead of only the first and last.
Nested regex walks now enforce branch-local `^` anchors.

Regression coverage is in
`t/oo/trait/handles-quoted-fat-arrow-rename.t` and
`t/regex/regex-anchored-alternation-subst.t`. The ecosystem ledger now records
the distribution as green: 1/1 baseline files and 1/1 assertions in parity;
the four native-library tests remain excluded because Rakudo cannot load their
missing library in the measurement environment.
