# A `Method`/`Regex` Instance from `.^lookup` answers `.arity`/`.count`

```raku
class B { method foo($a) {} }
my $m = B.^lookup("foo");
say $m.arity;
say $m.count;
```

`raku` prints `2` twice (the invocant plus the one declared parameter).
mutsu raised:

```
No such method 'arity' for invocant of type 'Method'
```

A Rakudo `Method` is a `Routine`, so it answers `.arity`/`.count` like any
other. mutsu already built a correct `signature` attribute for the
`Method` Instance `.^lookup`/`.^find_method` hand back
(`make_method_object_with_owner_ex`), but nothing computed the two derived
numbers from it — they aren't stored attributes, so the generic
auto-accessor fallback in `dispatch_instance_and_fallback`
(`src/runtime/methods_instance_ops.rs`) never served them.

Fixed by reading the `SigInfo` back off the already-materialized
`Signature` (`crate::value::signature::extract_sig_info`) and running it
through the same arithmetic the `Sub`/`Routine`-handle paths use
(`Interpreter::signature_required_positional_count` /
`signature_count_value`, `methods_signature_candidates.rs`) — so the two
numbers can never drift from the signature the object itself reports.

A grammar `token`/`rule`/`regex` reached through `.^lookup` shares the
exact same gap under a different class name (`Regex`, not `Method` — real
Rakudo's naming, verified against `raku`), but its `.signature` turned out
to be wrong too: `make_native_method_object_ex_loc` (which builds this
Instance) answered a generic single-argument-capture signature for
*every* grammar token regardless of its real declared parameters, the
same synthesized shape a genuinely native (Rust-implemented) method
answers when mutsu has no per-method fidelity override for it. Fixed by
threading the token's real `param_defs` (from its `FunctionDef` in
`Registry::token_defs`) through the same invocant + `*%_` construction
`make_method_object_with_owner_ex` already uses for ordinary methods,
so `grammar G { token foo ($x) { ... } }; G.^lookup("foo").signature.raku`
now answers `:(G $:: $x, *%_)` — matching `raku` exactly — instead of a
generic `:(G $:: |)`, and `.arity`/`.count` inherit the fix for free since
they read the same signature.

`t/oo/method/method-object-arity-count.t` covers both class shapes: a
required param, a required+optional mix, a slurpy (`count` going to
`Inf`), a zero-param method, a token with a declared param, a token with
none, and confirms an ordinary native method's signature read still lives
unaffected.

[#8416](https://github.com/tokuhirom/mutsu/issues/8416)
