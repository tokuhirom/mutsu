# `{*}` is resolved from the callers, not from where it is written

mutsu used to rewrite a `{*}` into the proto dispatch only where it sat
textually inside a proto body, with a static rule that turned the closure
literals in a method call's arguments into `Nil`. Rakudo has no lexical notion
of where a `{*}` belongs: it asks the dynamic scope for the nearest caller that
has a dispatcher. So `sub f2 { {*} }; proto f($) { f2() }` dispatches `f`, a
closure kept in a variable and handed to `map` gives `Nil`, and a `{*}` with no
dispatcher anywhere in the call chain dies with `X::NoDispatcher` -- mutsu
printed `*` for all of these (#10746).

The parser now reads the onlystar term `{*}` -- spelled exactly so; `{ * }` is
a block returning `*` -- as the dispatch call wherever it appears, and the
static rewrite of proto bodies is gone. At run time the call is resolved by
`Interpreter::resolve_onlystar` (`src/runtime/onlystar.rs`) from three O(1)
facts: the innermost proto body, the multi and wrap deferral frames entered
since (compared by the shared dispatch token), and the number of method calls
in progress. Each VM method-call opcode, a proto's own redispatch to its
winning candidate, and the forcing of a lazy list or a deferred `.map` count as
a method call, so a `{*}` reached through any of them is `Nil`, while a plain
`sub` or block in between is looked through.

Along the way two spellings now match rakudo: `{ * }` inside a proto body is a
block that returns `*`, and a bare `*` statement there is a `Whatever`, not
the dispatch.
