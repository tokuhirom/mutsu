# An imported proto named like a core routine dispatches through its code value

A package-less module can export a `proto`/`multi` family under the name of a
core routine. P5reverse, for example, exports its own `reverse`. A direct call
such as `reverse('Bar')` already reached the imported candidates. Calling it
through the code value, as in `'Bar'.&reverse`, `&reverse('Bar')` or
`my &r = &reverse; r(...)`, still ran the core `reverse`.

The cause is in the dispatcher that the code value carries. It re-dispatched
by name through `call_function`, the builtin funnel, and that function's
arms run a core routine before they look at any user declaration. Such a
dispatcher now goes through `call_function_fallback` whenever the name is
also a core routine. That path ranks user candidates ahead of the native
table, exactly as the direct call does.

As a result, P5reverse's `t/01-basic.rakutest` passes in full under mutsu.
