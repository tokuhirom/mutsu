# `Grammar.parse` honours a `.wrap` on the start rule

`G.^find_method('TOP').wrap(...)` was silently ignored by `.parse`,
`.parsefile` and `.subparse` (issue #9190), while a wrap on a token reached as
a subrule (`<word>`) already worked. `dispatch_package_parse` matches the
start rule's pattern directly and never consulted the method wrap table.

The start rule now goes through the same wrap-dispatch frame the subrule path
uses (`call_wrapped_token_method_with_terminal`). The wrapper receives a
cursor at the start position; the synthetic terminal its `callsame` reaches
carries the original `.parse` call and re-enters `dispatch_package_parse` with
a one-shot bypass flag, so the whole regular parse -- full-match anchoring,
`:actions`, proto candidates, failure reporting -- runs unchanged inside the
wrapper, and the wrapper's return value is the parse result. The same applies
to a non-`TOP` start rule chosen with `:rule<...>`.

The companion issue #9189 (a wrap installed after a grammar has parsed is
ignored by Rakudo) was closed without a code change: Rakudo's behaviour comes
from a per-callsite method-dispatch cache that ignores late wraps for plain
class methods too, while `Routine.wrap` is documented to affect every call.

Pinned by `t/grammar/grammar-wrapped-token-dispatch.t`.
