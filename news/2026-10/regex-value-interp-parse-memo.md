# `<$rx>` reuses its parse while the Regex value stays the same

A pattern that splices a Regex value, such as `$line ~~ / <$rx> /` in a loop, was parsed afresh on
every match. The `<$var>` arm reads the variable's value during the parse, so every text-keyed parse
memo refused the result. Before ADR-0135 Slice C part 2 that cost only the parse. Once `<$rx>`
compiled to the regex VM, each fresh `RegexPattern` also compiled a new `RxProgram`, probed its ASCII
acceptance tables and built its prefilter. All of these are meant to be built once per pattern, and
`bench-regex-match` rose 39% in instructions (#10716).

The parse depends on a small set of inputs: the interpolated text, the package, the token generation
and the bindings of the variables the arm read. `src/runtime/regex_value_keyed_parse.rs` records
those reads while a top-level parse runs. It stores the tree keyed by them, and a later parse reuses
the tree, along with its compiled program and tables, while every variable is still bound to the
same value. A read is keyed only when its effect on the tree depends only on the value's identity.
That means a Regex value whose own pattern text is static. A Str value, a Regex that interpolates
further variables, or any other ambient read leaves the parse unstored, as before.

Under callgrind, the issue's repro (3000 matches of `/ <$rx> /`) went from 1,080M to 151M
instructions. `benchmarks/bench-regex-match.raku` went from 2,490M to 1,376M, which is below its
level before the regression.
