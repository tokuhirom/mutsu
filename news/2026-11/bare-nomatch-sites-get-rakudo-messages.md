# Every built-in NoMatch names the call and lists rakudo's candidates

The 38 call sites of `make_multi_no_match_error(method)` raised `No matching candidates for method:
<name>` -- no invocant, no call profile, no candidates. The helper is gone. A user-visible error is
built with `builtin_method_no_match(target, method, args)` (rakudo's `Cannot resolve caller` header,
candidates from the generated table) or `Interpreter::user_method_no_match` (a user class's own
candidates; a mixin lists its roles' candidates on the mixin type, `(Int+{R} $:: Int $x, *%_)`; a
proto with no candidates says "Routine does not have any candidates.  Is only the proto defined?").
The three places that use the error only as a signal between dispatch layers take
`multi_no_match_signal`, which formats nothing (the user-`new` fallback of #9494 stays cheap).

Along the way: `scripts/builtin-method-sigs.raku` now reads role groups through their pun, so
`Buf.allocate`, `Real`, `Positional` and the other role methods have candidate lists; a `Nil` argument
is profiled `Nil:U`; `"a".subst-mutate(...)` reports that its candidates "require mutable arguments";
and `$*OUT.printf` without a format says "Too few positionals passed", as rakudo's routine does.
Closes #17859.
