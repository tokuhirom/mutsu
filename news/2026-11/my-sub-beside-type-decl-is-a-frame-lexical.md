# A `my sub` beside a lexical type declaration stays invisible to a callee's EVAL

The frame-lexical binding of `my sub`s (ADR-0113) was switched off for the whole
routine as soon as its body declared a `class`, `role`, `enum` or `subset`, so
the sub went through the flat routine registry and a callee's `EVAL 'helper(5)'`
could call the caller's `my sub helper`. Only a `my sub` whose declaration
mentions one of the body's own types needs the per-call type resolution; the
others are frame lexicals again. A `package` or `augment` still disables the
binding. A `my sub` that mentions a local type is still registry-resolved
(#16099 remainder).
