# A constant term in a signature beats a same-named type in the script

`multi infix:<*>(Int $n, G)` in a module, where `G` is the module's exported
`constant`, never matched when the importing script declared a `class G` or
`grammar G`: the bare name resolved to the script's type and the candidate was
skipped. The constant now wins, as the lexical scope the signature was written
in decides on Rakudo. EC's `secp256k1` fell back to the generic double-and-add
candidate and took 8 s instead of 0.09 s per point multiplication (#15981).
