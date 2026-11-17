# Linux::NFTables t/02 and ParaSeq t/08, t/20 are accepted divergences

On 2026-11-17 the maintainer decided that three ecosystem test files are accepted divergences
(ADR-0130). Each fails under mutsu only because of a timing artefact in the distribution or its
native library, and rakudo fails the same way once that timing is forced.

- **Linux::NFTables `t/02-oo.rakutest`** (#17834). libnftables double-frees a context after
  `nft_ctx_clear_vars`. mutsu runs the class's `DESTROY` when the object leaves scope, and rakudo
  dies identically after a forced GC. rakudo passes only because MoarVM never collects the object
  before exit.
- **ParaSeq `t/08-grep.rakutest` and `t/20-unique.rakutest`** (#17723). These hit ParaSeq 0.2.7's
  own `close-queues` race, which deadlocks on raku 2026.11 when a short delay is injected.

The heap corruption in the NFTables case prints a different first line on each run. So an entry's
`first_failure` may now be a list of alternatives, each an exact line or a whole-line `re:`
pattern, while every other shape field is still compared exactly.
