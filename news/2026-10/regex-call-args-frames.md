# Subrule calls with arguments run on the compiled regex engine

A `<rule(…)>` call with arguments always handed the call back to the tree walk. The compiled regex
engine (ADR-0135) now evaluates the arguments once at the call, resolves the rule for those values
and runs it as a frame of its own loop, as it already did for an argument-less call. A call that
still needs the walk (a `$*` parameter, a wrapped token, a method of that name) is handed the
evaluated arguments instead of evaluating them again.

Part of [#10255](https://github.com/tokuhirom/mutsu/issues/10255).
