# Encoding::Emacs is green in the ecosystem parity ledger

`Encoding::Emacs` v0.1.5.1 now passes all 13 of its Rakudo-baselined test
files under mutsu: 453/453 assertions, with every provided module loading.

The fixes were general interpreter behavior:

- `proto regex` declarations now consume the `regex` declarator and register
  the actual proto name, allowing multiple proto regexes in one grammar.
- Compound assignment expressions write back class-body and package lexicals
  to their authoritative package store.
- `Encoding::Decoder` and `Encoding::Encoder` are available as composable
  roles, matching the streaming interfaces used by the distribution's
  Registry bridge.

Pinned by `t/grammar/multiple-proto-regex.t`,
`t/oo/class/class-body-static-compound-assignment.t`, and
`t/oo/role/encoding-interface-role-composition.t`.
