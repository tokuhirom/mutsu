# A written security model and a security-audit skill

mutsu had one security rule (do not weaken hardening without approval) and no description of
what it trusts. `docs/security.md` now states the threat model: running a Raku program is trusted,
so security work is about the surfaces where something less happens and must not turn into
running code. Those surfaces are analysis and the language server, module resolution, caches,
files the runtime writes on its own, child-process fds, data decoders, the playground, CI secrets
and agent sessions. Memory safety comes on top, because even a trusted script must not be able to
corrupt the heap. Each surface gets explicit invariants and the tools that check them: cargo-audit,
zizmor, the unsafe-documentation clippy lints, sanitizers, Miri and fuzzing.

AGENTS.md gains a hard rule that forbids adding a new crossing of those boundaries. The new
`security-audit` skill gives the audit procedure. For each surface it lists the grep inventory, the
proof-of-concept shapes and the pass conditions. It also says how findings are reported and filed.
The skill was written after a first full audit with that procedure. That audit found, among other
things, cross-thread container mutation reachable from plain `start` blocks, and parse-time module
probes that run code under `--dump-ast` and in the language server.

External skills were surveyed first. Trail of Bits' `rust-review` and related skills are the
closest match, but they are CC BY-SA 4.0, so they are referenced as an optional plugin and not
copied in. Claude Code's built-in `/security-review` remains the diff-level pass.
