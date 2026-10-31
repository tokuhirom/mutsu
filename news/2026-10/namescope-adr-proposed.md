# A Proposed ADR for NameScope and build-time lowering

ADR-15760 proposes how the executing path stops being three passes (the spelled parse, `finalize_unit`, a second
`qualify_type_names`): one `NameScope` value owned by the parser, passed to every lowering, and each typed node
lowered where the parser builds it. It records the measured pipeline, what deleting `unspell`, the second
qualification pass and the `round_trip*` names depends on, and three open questions for the maintainer. Nothing in
`src/` changes until it is Accepted (RakuAST C2, #15760).
