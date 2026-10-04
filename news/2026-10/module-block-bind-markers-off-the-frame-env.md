# A `module` block's `:=` markers no longer leak into the importer's env

A `:=` binding records its shape — "decontainerize on read", "has no
container" — in companion markers keyed by the bound name. Their keys contain
`::`, and a `package`/`module` block's exit carries every such key out of the
block on the assumption that it is package-qualified, so a block's own
`my $x := …` left its markers behind while the binding itself was dropped.
`use JSON::Fast` left 20 of them in every frame env of the program.

The block's exit now drops those markers along with the binding. A
`Promise(supply { whenever … })` loop under `use Cro::HTTP2::RequestParser`
deep-copies 19% fewer env entries per iteration (3,195 → 2,595). This is the
sixth slice of ADR-0084 (#7817).
