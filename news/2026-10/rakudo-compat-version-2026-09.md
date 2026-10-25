# `$*RAKU.compiler.version` is now v2026.09

`Interpreter::RAKUDO_COMPAT_VERSION` moved from `2026.07` to `2026.09` (ADR-0104,
#14851), after the roast reference baseline (`TODO_roast/raku-baseline.tsv`) was
re-captured on Rakudo 2026.09 so the claim is backed by a measurement on both the
ecosystem and the roast side. Gates such as `static`'s
`die ... if $*RAKU.compiler.version < v2026.09` now take the modern branch. The
`Perl6::SysConfig` `version` key and the 6.e core type list
(`src/core_type_names_6e.txt`) follow the same release.

`scripts/roast-raku-baseline.sh` gained `RAKU_BASELINE_JOBS` for a parallel
capture; its output is sorted by path and does not depend on the job count. The
v2026.09 refresh notes are in `TODO_roast/raku-baseline.md`.
