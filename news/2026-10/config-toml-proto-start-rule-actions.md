# Config::TOML: start-rule proto actions and Match is not Associative

Two interpreter gaps found by taking the Config::TOML ecosystem distribution from partial to
all 19 test files passing. A grammar parse that starts at a proto token (`:rule<string>`) now runs
the winning candidate's action for the bare-adverb (`token s:plain`) and `:<x>` spellings, not only
`:sym<x>`. And a `Match` no longer satisfies `Associative` (rakudo: a Capture is not Associative),
so `Associative:D` multi candidates skip it.
