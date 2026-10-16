# A block-scoped import of built-in operator candidates keeps the built-in

`{ use M; -$p }` where `M` exports `multi prefix:<->(Pt $a) { ... -$a.v ... }`
died with "Cannot resolve caller prefix:<->(Int:D)": the block-scoped import
bound the family as a dispatcher over the module's candidates alone, so the
candidate's own `-` on an Int had nowhere to go. A family that adds
candidates to a built-in operator now stays on the registry path, where the
operator finds both the built-in and the imported candidates (ADR-8751
stage 2, #8751).
