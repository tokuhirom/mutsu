# ADR-15762: slangs by execution (Proposed)

Slang::Tuxic's three rules were a flag each; eleven more slang distributions (Qwiratry, Roman,
NumberBase, Date, Lambda, Emoji, Nogil, Mosdef, SQL, Otherwise, T) pass on rakudo 2026.09 and die
at activation on mutsu, each with a different override. ADR-15762 supersedes ADR-0026 §4: the Rust
parser keeps parsing, but a slang's own tokens and actions run on a slang host and plug into
production hooks, with native shims for the core rules a token calls and the attached RakuAST node
lowered in place (#15762).
