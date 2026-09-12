# Multi dispatch stops evaluating parameter defaults that nothing reads

A `multi` whose candidates carry non-constant parameter defaults warned once per
call about a `Nil` that never appeared in the result:

```raku
multi m(Int $c, $d = $c * 2)   { "int:$c/$d" }
multi m(Str $c, $d = $c ~ "!") { "str:$c/$d" }
say m("a");
```

```
Use of Nil in string context
  in block <unit> at nilwarn.raku line 3
str:a/a!
```

The value was right; only the warning was wrong. The same signature written as a
plain `sub` was silent, which pointed at the dispatch path rather than at the
default itself ([#8078](https://github.com/tokuhirom/mutsu/issues/8078)).

## The evaluation had exactly one consumer, and it was optional

`Interpreter::args_match_params` checks an *unsupplied* parameter against the
value it would actually bind, so that dispatch and binding agree — a candidate
selected on its defaulted parameter must not then die binding the very call it
was selected for ([#8089](https://github.com/tokuhirom/mutsu/issues/8089)). Part
of that is evaluating the parameter's default so a `where` clause can be tested
against it.

The evaluation was unconditional: every unsupplied parameter with a default had
it evaluated on every dispatch, whether or not the parameter carried a `where`
clause — and `where_default` was read in exactly one place, inside
`if let Some(where_expr) = &pd.where_constraint`. For a candidate with no
`where`, the result was computed and immediately dropped.

Dropping it silently would have been merely wasteful. Dropping it *loudly* is the
bug: matching runs before the arguments are bound to parameter names, so a
default that reads an earlier parameter (`$d = $c ~ "!"`) saw an unbound `$c`,
took the `Nil` string-context path, and printed the warning that path always
prints. `$c * 2` was unaffected only because numeric context does not warn — the
Int candidate was evaluating a bogus default too, just quietly.

The fix gates the evaluation on `pd.where_constraint.is_some()`. Nothing else
read it, so nothing else changes: a candidate is still selected on its defaulted
`where`-parameter (pinned by the existing
`t/routines/signature/optional-param-where-runs-when-omitted.t`, and again here),
and every other candidate now pays no default evaluation at dispatch time at all.

## Why the regression test watches STDERR

The obvious pin — a `CONTROL { when CX::Warn {...} }` counter around the call, as
`t/types/nil-str-context-warning.t` uses — counts **zero** on the broken build.
The warning is raised on the dispatch path, outside the caller's handler scope,
so an in-process handler never sees it while the text still reaches the terminal.
`t/routines/dispatch/multi-nonconstant-default-no-spurious-warn.t` therefore runs
each snippet as a subprocess and asserts on its STDERR, the way
`t/modules/module-parse-warning-once.t` does. On the unfixed binary it fails on
exactly the two STDERR assertions; under `raku` it passes as written.

That the handler cannot see this warning is itself worth knowing: a warning
emitted during candidate matching is currently unsuppressable from Raku code
(`quietly` would not have silenced it either). Removing the only known source of
one was the right first move; if another turns up, the handler scope on the
dispatch path is the thing to fix.
