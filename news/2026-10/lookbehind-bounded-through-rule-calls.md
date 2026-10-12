# A look-behind over a rule call searches back only as far as the rule reaches

`<?after X>` / `<!after X>` is answered by running `X` forward from candidate
starts until one ends at the cursor. Since #7576 the search starts at the
cursor minus the most `X` can consume, but a body that called a rule was not
bounded, so it tried every start from the beginning of the string. YAMLish's
`plain` scalars evaluate `<!after <.space>>` — `space` being the grammar's own
`token space { <[\ \t]> }` — once per character, so each evaluation cost
O(position) and a document cost O(n²): the superlinear term the
`bench-yaml-parse-big` header measured.

The bound now follows rule calls into the bodies the engine runs: a call to a
rule is as long as the longest of its candidates' bodies (through up to eight
nested calls), and a builtin character class such as `<.alnum>` is one
character. A call the engine answers with user code (a grammar method, a
wrapped token, a custom HOW), a call with arguments, and anything recursive or
unbounded still searches from the start, as before.

On the 60-row benchmark document one warm `load-yaml` went from ~646M to
~294M instructions (-54%; release, `MUTSU_JIT=off`), and the section time from
~0.165 s to ~0.108 s. Growth with document size is now linear: 60, 240 and 640
rows parse in 0.15, 0.75 and 1.45 s, where they took 0.22, 1.76 and 6.46 s.
