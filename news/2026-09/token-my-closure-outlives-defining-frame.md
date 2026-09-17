# A my token/rule storing &NAME past its declaring frame keeps its interpolated lexicals

A `token`/`rule` declaration that interpolates a dynamic pattern
(`<{$var}>`, `<$var>`) and is stored into an attribute via `&NAME` for
later use -- outliving the method call that declared it -- lost the
captured lexical's value by the time it was actually matched. The regex
behaved as if compiled from an empty string, throwing "Null regex not
allowed" instead of matching normally.

`Compiler::regex_literal_closure_captures` already fixed this for a
`/regex/` literal assigned to a variable, but only that expression-compiled
path emits `OpCode::LoadRegexClosure` to snapshot the defining frame's
lexicals. A named `token`/`rule` declaration never reaches it: ADR-0009
keeps its body a raw, interpreter-executed `Stmt::Expr(Expr::Literal(regex
value))` payload, whose embedded regex literal's `scope` stayed `None`
forever, baked in at parse time.

mutsu now computes the same capture list at token/rule declaration compile
time and fills it in at registration time, and the smartmatch dispatch for
a named-token RHS installs that captured scope for the duration of the
match, the same way a directly-stored regex literal's closure scope is
installed.

Found via Router::Right's incrementally-built route-matching regex
(`submethod !add` builds `$fullre` and stores `my token FULLRE { ^
<{$fullre}> }` as `&FULLRE` into an attribute, matched much later from a
public `.match()` method).

Pinned by `t/grammar/token-my-closure-outlives-defining-frame.t`.

Closes #8662.
