# The `regex` crate is no longer a runtime dependency

mutsu has its own Raku regex engine, but it still linked the `regex` crate
(with `regex-automata`, `aho-corasick` and `memchr`) for jobs that never needed
a matching engine. All of them are gone (#10439):

- **Unicode property tests** (`<:Prop>`, `<:sc<Latin>>`, `.uniprop`, `unimatch`,
  the UAX #29 segmentation properties) compiled `^\p{..}$` and matched a
  one-character string. `builtins::unicode_prop_class` now resolves the class
  to its codepoint ranges once per name through `regex-syntax` (which has no
  dependencies) and answers by binary search; `Nd`/`No`/`Nl` checks use the
  committed General_Category tables directly. A test pins the answers against
  the old regex probe, including the classes `regex-syntax` folds to a single
  codepoint (`Zl`, `Grapheme_Cluster_Break=LF`).
- **`<:name(/.../)>`** used to translate the argument into a Perl-style regex.
  Rakudo smartmatches the character's name against it, so the regex now runs
  on mutsu's own engine (`Interpreter::unicode_property_holds`), as an atom and
  as a `<+:name(...)>` class item; a string argument compares for equality, as
  Rakudo's smartmatch does (`<:name("LATIN")>` no longer matches every Latin
  letter). The scan prefilter treats a Name regex as opaque.
- **The parser's source-text fallbacks** for module exports (`sub ... is
  export`, unit-scope routines, `sub EXPORT`) are hand-written scanners in
  `module_exports/source_scan.rs`, pinned against the old patterns over every
  module source in the repository. They now skip Pod blocks and heredoc
  bodies, so a `sub EXPORT` quoted in documentation no longer switches on the
  export-hook approximation.
- **Two string-level splits in the regex parser** (`atom ** N..M % sep` and the
  `<rule(...)>` code check) are hand-written in `runtime/regex_ltm_split.rs`,
  checked exhaustively against the old patterns over short strings.

`regex` remains a dev-dependency for those equivalence tests.
