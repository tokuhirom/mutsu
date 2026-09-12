# A type capture is its own field on `ParamDef`, so `::T Foo:D:` parses and `T` binds

`method merge(::T CRDT:D: $other --> T)` — a type capture on the invocant *followed by* a nominal
invocant type — did not parse at all, and the two spellings that did parse mis-bound the captured
name. All three problems were one representation bug: `ParamDef` carried a parameter's type as a
single `Option<String>`, and a capture was encoded *inside* that string as `"::T"`. There was no room
in it for both `::T` and `CRDT:D`.

```
$ mutsu -e 'class Foo { method m(::T Foo:D: $x --> T) { $x } }; say "ok"'       # before
===SORRY!=== Error while compiling -e
Confused. expected statement: ...
$ mutsu -e 'class Foo { method m(::T: $x --> T) { $x } }; say Foo.new.m(Foo.new).WHAT'
Type check failed for return value; expected T but got Any (Foo())
```

`ParamDef` now has a `type_capture: Option<String>` of its own, holding the bare captured name with
no `::` prefix and no smiley; `type_constraint` is only ever the nominal half. One accessor,
`ParamDef::captured_type_name()`, is what every binding, dispatch, role and RakuAST site asks —
roughly twenty of them used to re-derive the capture by stripping `"::"` off the constraint string,
which is exactly why a parameter could never hold both facts. The parser builds the pair at one
choke point: the capture branch consumes `::T` and then parses whatever follows as an ordinary
parameter, hanging the capture off the result, so defaults, traits, `where` clauses and
sub-signatures all keep it for free. An invocant capture is split off before the invocant marker is
read (`split_invocant_type_capture`), so `::T CRDT:D:` becomes one invocant parameter carrying both
`type_capture: Some("T")` and `type_constraint: Some("CRDT:D")` — the capture binds *and* the
nominal type is enforced, where the old code had them as mutually exclusive arms of one `if`/`else`
chain.

Three further defects fell out of the same change.

**The capture-bound marker collided with a parameter name.** `bind_type_capture` recorded "a capture
named `T` is bound" under the env key `__type_capture__T` — which is also the synthetic *parameter
name* a bare `::T` / `::T:` capture carries. So the binder's own "bind the parameter under its name"
step overwrote the marker with the argument value, `has_type_capture_binding("T")` then said no, and
every later resolution of `T` fell back to the literal name. That is why `--> T` reported "expected
T". The marker key is now `__mutsu_type_capture_bound__<name>`, which no parameter name can reach.

**A method resolved its `--> T` return constraint after its own env was gone.** The sub paths
resolve the return spec before popping the call frame; both method paths did it after restoring the
caller's env, so a capture bound by the method's own signature was no longer visible. Both now
resolve it while the callee env is still live.

**A `::T` capture on a method silently took the read-only fast path.** The fast path has no
capture-binding step; it used to be kept out by accident, because the capture lived in
`type_constraint` and either the invocant-constraint gate or the fast path's own nominal type check
(which `"::T"` could never satisfy) bounced the call. With the capture in a field of its own the
gate has to name it, so `has_type_capture` joins the fast-path conditions. Before this,
`class Foo { method m(::T $x) { say T } }` failed outright — the capture was type-checked against
the literal string `::T`.

**A role body that does not parse is now a parse error.** `role_decl` fell back to
`consume_raw_braced_body` on a non-fatal block failure, which discarded the entire body and produced
a role with no methods. `role R { method m(::T R:D: $x) { 1 } }` therefore compiled silently and the
call site died with "No such method 'm'", with nothing pointing at the signature that had failed to
parse. Roles now parse their body exactly as classes do.

Signature introspection follows the field. `SigParam` grew the same `type_capture`, so
`Parameter.type_captures` reads it there rather than by slicing a `"::T"` constraint string,
`.type` stays `Any` for a capture-only parameter, and `.gist` renders the capture ahead of the
nominal-type slot the way Rakudo does — two spaces and all (`(::T  $x)`), with a bare `::T`
parameter showing as the anonymous `$` instead of leaking its synthetic
`$__type_capture__T` name.

Several fast paths had been excluding captures *by accident*, because `"::T"` was a constraint string
that no fast type check could satisfy. With the capture in a field of its own they had to be told:
the method fast path (above), and the sub light / positional-light paths in `vm_call_eligibility.rs`.
Getting that wrong was visible immediately — `sub f(::T $x) { my T $y = 4.2 }` reported "Type 'T' is
not declared", because the light path bound the argument without ever binding the capture.

The `::T:D` spelling is no longer read as a capture *named* `T:D`. Rakudo reports such a parameter's
`.type` as `Any` and enforces nothing — `sub f(::T:U $x) { }; f(42)` runs there — so the smiley is
consumed and discarded and the capture is named `T`, which is what `::GrammarType:U :$schema`
(YAMLish) has always meant.

Pinned by `t/routines/signature/invocant-type-capture-param.t`. Closes #7984. One adjacent pre-existing
divergence surfaced while writing that test and is tracked separately: a capture-constrained
parameter's binding failure is catchable by `try` but does not reach `dies-ok`'s handler (#8064).
