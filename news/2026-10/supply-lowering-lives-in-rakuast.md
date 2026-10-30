# The `supply` block lowering lives in rakuast

ADR-15334: `supply { ... }` was already the typed `ParsedExpr::Supply`, but the one lowering that
rewrites its body (`emit`, `done`, CLOSE phasers around the on-demand emitter) was still parser
code. It moved to `rakuast::supply_lower`; the parser keeps only the node constructor. The
`SyntheticBlock` sites of the `done` rewrite left `src/parser` (`synthetic-block-sites` 35 -> 32; `internal-name-literals` 58 -> 57).
