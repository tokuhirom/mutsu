# Capture.gist is its .raku form

`Capture.gist` kept a second renderer built on `to_string_value`, so a Pair, a nested list,
Bool/Nil and allomorph argument rendered differently from Rakudo (`\(1, 2, a<TAB>3)`). The gist
now delegates to `capture_raku`, the single renderer `.raku` uses (#12114).
