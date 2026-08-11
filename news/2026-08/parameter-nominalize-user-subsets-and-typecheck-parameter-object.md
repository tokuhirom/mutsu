# `Parameter.type`/`.constraints` nominalize user-declared subsets; every `X::TypeCheck::Binding::Parameter` now carries a real `.parameter`

`Parameter.type` previously only nominalized the builtin `UInt` subset to its
base `Int` — a user-declared `subset Odd of Int where * % 2; sub f(Odd $x)
{}` left `&f.signature.params[0].type` as the subset itself instead of `Int`,
and `.constraints` empty instead of `all(Odd)`. This blocked
`Cro::HTTP::Router`'s route compiler, which dispatches route-parameter types
via `$type =:= Int`-style checks and dies with "Parameter type ... not
allowed" on any subset-typed route parameter it cannot resolve to a nominal
type.

The blocker was plumbing, not semantics: `build_parameter_attrs` in
`src/value/signature.rs` is a static function with no access to the runtime's
subset registry. It now takes `Option<&Interpreter>`, threaded through the
whole `Parameter`/`Signature` construction chain
(`make_signature_value[_with_owner]`, `make_params_value_*`,
`sig_param_to_parameter_instance[_with_owner]`,
`make_parameter_value_from_param_def`) from every call site that has an
interpreter available — which is all of them except one: the parse-time bare
signature literal (`:(...)` as an expression) in
`src/parser/primary/misc/colonpair.rs`, which has no interpreter yet and
passes `None`, leaving that one construct's subset types unresolved as
before. A new `resolve_subset_base` walks a subset-of-subset chain to its
first non-subset base via `registry().subsets`, deliberately *not* reusing
`Interpreter::nominalize_type_name`'s `:D`/`:U`/coercion-stripping — that
would have changed unrelated existing behavior.

Testing this against `tmp/frameworks/cro/t/http-router.rakutest` (the Cro
campaign's vendored native-int/subset route-parameter suite) surfaced a
second, unrelated pre-existing bug: several `X::TypeCheck::Binding::Parameter`
construction sites in `src/runtime/types/binding_signature.rs` (all four
`where`-constraint binding-failure sites, plus the coercion-binding-failure
normalizer) built the exception with only a `.message` attribute, never
attaching a real `Parameter` object to `.parameter`. Consumer code that reads
`.parameter.named`/`.parameter.type` from a caught binding failure — exactly
what `Cro::HTTP::Router`'s bind-failure classifier does to pick a 400 vs 401
vs 404 response — crashed with "No such method 'parameter'". Once the subset
fix let the router test progress far enough to actually reach a `where`-only
positional-parameter binding failure (`get -> 'tag', $tag where /^\w+$/`
against a non-matching segment), this became reachable and visible. Fixed by
threading the same `pd`/`Option<&Interpreter>` pair into
`Interpreter::parameter_binding_error` and `normalize_coercion_binding_error`,
and converting two duplicated inline exception-construction blocks to call the
shared helper.

Together these took `http-router.rakutest` from dying at subtest 171 to
running cleanly through subtest 180, where it now hits a distinct,
newly-discovered hang (tracked separately —
`todo/tickets/cro-router-slurpy-where-clause-nonmatch-hangs.md`). `make test`
and the whitelisted `S06-signature`/`subset-6c`/`subset-6e`/`S04-exceptions`
roast files stay green.
