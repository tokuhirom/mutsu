# The last `src/runtime/` qualified-name string surgery is gone

The #11507 sweep finished `src/runtime/`'s top level: `regex_parse_*`,
`registry`, `resolution*`, `routine_candidate_defs`, `run*`, `runtime_*`,
`sequence_closure_call`, `system_eval_*` and `unit_private_routines`. That
removed 121 sites. The qualify and global-cmp counters of
`make check-name-scans` now read 0 across the whole tree. The scan counter
reads 8, all of them byte scans kept on purpose on `&str`-only hot paths.

What changed:

- Token proto MRO walks split their qualified name through `split_qualified`.
- The package-to-distribution walk follows `package_ancestors`.
- Module export and import aliasing, and the EVAL operator and sub-name
  collectors, take short names from `last_segment`.
- Routine candidate prefixes are built from the memoized `qualified_text`.
- The regex adverb check splits pattern text with `text_segments`, which does
  not intern it.
