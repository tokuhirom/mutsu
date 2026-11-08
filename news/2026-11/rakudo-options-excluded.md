# Rakudo::Options is not supported and is excluded from the ecosystem measurement

Maintainer decision (2026-11-08): Rakudo::Options is a rakudo-specific distribution, so mutsu does
not support it. Its `t/02-with.rakutest` asserts that MoarVM's specializer log (`MVM_SPESH_LOG`)
writes a file, an artefact mutsu will not fake. The distribution is listed in
`ecosystem/exclude.txt`, which removes it from the work queue and bulk sweeps (`--only
Rakudo::Options` still measures it). The other four of its test files already matched rakudo.
