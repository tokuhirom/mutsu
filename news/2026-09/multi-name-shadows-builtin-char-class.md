# A multi named after a builtin character class no longer swallows its own `X::Multi::NoMatch`

A `multi sub` whose name happened to coincide with a builtin regex character
class (`alpha`, `digit`, `xdigit`, `alnum`, ...) silently returned a `Regex`
for that character class instead of raising `X::Multi::NoMatch` when a
runtime-only call (`alpha(|@args)`, spread so the mismatch is discovered only
at dispatch time) matched no candidate.

The cause: `call_function_fallback`'s ordinary function-call path shared
`eval_token_call_candidates_at` with the legitimate callers that select a
builtin character class as a grammar start rule
(`Grammar.subparse(text, :rule<xdigit>)`) or as a `<alpha>` regex atom. Since
the failed multi has no user token definition either, the shared helper's
"no user token, but the name is a builtin character class" branch fired for
the ordinary call too, intercepting the dispatch failure before it could be
reported.

`eval_token_call_values`/`_at`/`eval_token_call_candidates_at`
(`src/runtime/dispatch.rs`) now take an explicit `allow_builtin_char_class`
flag. Only the grammar start-rule (`src/runtime/methods_grammar.rs`) and
regex-atom (`src/runtime/regex/regex_token_method.rs`) callers pass `true`;
the plain function-call fallback (`src/runtime/builtins_operators_fallback.rs`)
passes `false`, so a name with no user token and no builtin-character-class
license falls through to the ordinary `X::Multi::NoMatch` reporting.

Pinned by `t/routines/dispatch/multi-name-shadows-builtin-char-class.t`, which
also regression-guards the two legitimate fallback paths (#9719).
