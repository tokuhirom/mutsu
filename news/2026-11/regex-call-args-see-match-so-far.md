# A regex call's arguments see the match so far after a code block

In `token d { <[0..9]> { } <.h($/.Str)> }` Rakudo has published the cursor as `$/` by the time
the call's arguments are evaluated, so `$/.Str` is the text matched up to the call. mutsu built
`$/` for the argument list from the bare positional captures, so the call got the empty string.

A code block (plain `{ }` or an assertion `<?{ }>`) now leaves a marker in the rule frame's
in-regex variables, and the compiled engine evaluates a later call's arguments
(`eval_regex_call_args`) with `$/` set to the cursor Match, built the way an inline block builds
it. Without a block the arguments still see the unset `$/`, as in Rakudo, and a block in a callee
does not reach its caller's frame (#16308, found with Intl::Format::Number).
