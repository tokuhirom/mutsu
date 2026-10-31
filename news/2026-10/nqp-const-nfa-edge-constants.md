# `nqp::const::EDGE_*` constants and `when nqp::const::NAME { }`

CoreHackers::NfaChainsaw's `NfaRunner` dispatches NFA edges with
`when nqp::const::EDGE_FATE { ... }`. mutsu had no QRegex::NFA edge constants, and the parser
read a `nqp::const::` name after `when` as a routine gobbling the block, so the module did not
parse. The 24 `EDGE_*` values now fold to integer literals like the other `nqp::const::` names
(values taken from rakudo), and a `nqp::const::` name is a complete term after `when`.
`NfaRunner` now loads like rakudo. `Extractor` (`use QRegex:from<NQP>`) still does not.
