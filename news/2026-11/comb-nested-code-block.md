# `.comb` runs a regex's code blocks inside groups and alternations

`.comb` with a regex takes a fast, non-capturing search unless the pattern needs its code run;
that decision looked only at the pattern's top-level tokens. A code block inside a group or an
alternation (`"aa".comb(/[ a { $n++ } | b ]/)`) was therefore never run -- the matches were right,
but every side effect of the block (a counter, a `take`, a variable a later step reads) was
silently lost (#18529). The decision now uses the memoized whole-pattern check
(`pattern_contains_code`) that already sees nested groups, alternatives and separators.
