# A parameter may carry a custom `is` trait

`sub search(:$min-price is query = 0) { }` was a fatal parse error —
`Can't use unknown trait 'is' -> 'query' in a parameter declaration` — because
the parser matched every parameter trait against a fixed list of six names
(`rw`, `readonly`, `copy`, `required`, `raw`, `encoded`). Raku has no such list:
a parameter trait is legal whenever some `trait_mod:<is>` candidate accepts a
`Parameter`, and `Cro::HTTP::Router` declares four of them —

```raku
multi trait_mod:<is>(Parameter:D $param, :$query! --> Nil) is export {
    $param does Cro::HTTP::Router::Query;
}
```

— for `query`, `header`, `cookie` and `auth`.

Whether a name is legal is not knowable at parse time, so the check moved to the
declaration site, mirroring how a *sub*-level custom trait is already handled in
`exec_register_proto_sub_op`. `Interpreter::check_param_custom_traits` walks the
`ParamDef`s (recursing into sub-signatures), builds a real `Parameter` value for
each parameter carrying a non-built-in trait, and dispatches
`trait_mod:<is>($param, :$name)`. A dispatch failure is reported as raku's
unknown-trait error, so `sub oh-noes($gack is nonesuch) { }` still dies with a
message naming the trait (`roast/S06-traits/misc.t`). It runs at every site that
turns a signature into a callable: `RegisterSub`, `MakeLambda`, and
`MakeAnonSubParams` — the last is what a bare `-> ... { }` in argument position
compiles to, which is exactly the shape of a Cro route handler.

Two supporting changes:

- `parse_for_params` (loop variables, and a bare pointy block in argument
  position) keeps a parse-time check, because a loop parameter's traits are
  lowered away and never reach a declaration site. It now accepts an unknown
  name once the parser has seen a `trait_mod:<is>` declaration or import — the
  parse-time approximation of "some candidate might accept it".
- An imported `trait_mod:<is>` is now registered into the importing scope's
  known-sub set. It is not an operator sub, so it needs none of the
  precedence/term machinery `is_operator_sub_name` gates; it just has to be
  *visible* for the check above.

This clears the parse barrier on the upstream Cro::HTTP router suite:
`router-auth.rakutest` now runs instead of failing to compile, and
`http-router.rakutest` and `http-router-named-urls.t` get much further.

Still missing, and tracked in
`todo/tickets/parameter-trait-mixin-does-not-persist.md`: the `does` mixin a
trait applies is dropped, because `.signature.params` re-materializes a fresh
`Parameter` from the `ParamDef` on every access. Cro reads the mixin back when
generating a route matcher, so `is query` parameters do not yet route.

Pinned by `t/custom-parameter-trait.t`, which passes under raku too.
