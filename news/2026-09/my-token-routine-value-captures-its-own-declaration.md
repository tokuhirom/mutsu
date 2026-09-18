# A `my token`/`rule`'s `&NAME` reference now carries its own declaration's identity

Two objects of the same class, each declaring `my token FULLRE { ... }` under
the same short name inside a method, used to clobber each other's captured
`&FULLRE` reference. mutsu's `token_defs` registry is keyed by
`package::name` alone, with no notion of "this particular closure creation"
versus "that one" — a non-`multi` declaration *replaces* whatever is
currently registered under that key (`Registry::insert_token_def`). Since a
`my token` declared inside a method registers under its enclosing class's
package (not a per-instance key), the second object's `method build` call
re-declared `FULLRE` under the exact same registry key as the first object's
declaration, silently overwriting it. A `&FULLRE` reference captured by the
FIRST object then resolved, at match time, to the SECOND object's token
instead of its own (issue #8680).

Real Raku has no such problem: each `my token` declaration is a genuinely
fresh, lexically-scoped code object per closure creation, not a
global-registry entry keyed by name.

This follows on from #8662's fix (closure captures for a `my token`/`rule`'s
interpolated lexicals), which filled in *a* captured scope correctly but did
not give the `&NAME` reference itself any identity beyond a bare name —
every match still re-resolved that name against whatever was CURRENTLY
registered, so the most recently declared token under a name always won.

Fixed by giving `Value::Routine` (the `&NAME` reference's own representation)
an optional `captured_regex: Option<Arc<Value>>` payload: a direct pointer to
the regex value (carrying its own closure-captured lexicals) that the
reference resolved to at the moment it was created
(`Interpreter::accessors_resolve`'s `&NAME` term evaluation, via the new
`Value::routine_token_capture` constructor). The three real match-dispatch
consumers — smartmatch (`~~`), `.match`, and `.subst` — now prefer this
captured identity over a fresh `token_defs` name lookup, so a reference keeps
resolving to the exact declaration it was bound to regardless of what gets
registered under that short name afterward. A proto/multi token name still
falls back to the lazy by-name form, since LTM must still pick among several
candidates at match time and there is no single declaration to capture.

Regression test: `t/grammar/token-my-two-instances-same-name.t`.
