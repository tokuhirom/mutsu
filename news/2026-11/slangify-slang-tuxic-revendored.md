# Slangify 0.0.5 and Slang::Tuxic 0.0.6 are the bundled slang pair

The slang-activation batteries are re-vendored from their latest fez releases:
`modules/Slangify/` now carries `Slangify:ver<0.0.5>:auth<zef:lizmat>` (tag
`0.0.5`, commit `471f23d5`) and `modules/Slang-Tuxic/` carries
`Slang::Tuxic:ver<0.0.6>:auth<zef:raku-community-modules>` (tag `0.0.6`,
commit `d6b458ce`), both copied verbatim. They move together because
`Slang::Tuxic` is built on `Slangify` and they share one selection record.

Neither release changes what mutsu executes. `Slangify` added coverage markers
and moved its home to Codeberg (the GitHub mirror still carries the tag, so
the release gate keeps fetching from there); `Slang::Tuxic` now hands
`$*R.is-identifier-type` a plain string, inside the `term:sym<identifier>`
override that mutsu maps onto its spaced-call parser mode (ADR-0026).
`batteries.lock` pins both commits, and `docs/batteries/slang-tuxic.md` gains
a provenance table.
