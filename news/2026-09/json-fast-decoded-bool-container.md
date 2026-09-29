# JSON::Fast decoded Bool values are container-held

`nqp::p6scalarwithvalue` returned a bare `Bool`, so a Hash built by the vendored `JSON::Fast`
(`nqp::bindkey($hash, $key, nqp::p6scalarwithvalue(...))`) rendered `:false1` / `:!false1` in `.raku`
where rakudo, whose Scalar-held value renders `:false1(Bool::False)`, differs. A Bool is now
wrapped in a Scalar like the other stored values. `JSON::Hjson`'s `t/02-testcases.t` compares its
own output against `from-json` results via `.raku` and goes from 44/47 to 47/47.
