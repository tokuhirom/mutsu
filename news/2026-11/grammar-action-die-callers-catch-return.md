# A caller's `CATCH { return }` handles a grammar action's `die`

An action runs while the match is still in progress, so a caller's `CATCH
{ default { return … } }` handles its `die` right there and the `return` is a
non-local one, aimed at the caller. The reduction-time dispatch dropped every
`return` signal as if it were the action's own result, so the parse carried on
and answered normally, and the caller's `return` never happened.

A `return` stamped for an enclosing routine is now kept like the exception is,
and leaves `.parse`; an action's own `return` is still its result. Template::HAML's
`t/0810-attrs-parse-error.rakutest` (0 of 14) and the rest of its test files
match rakudo, apart from the cross-process precompilation test.
