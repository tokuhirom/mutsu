# A proto token's own `$*` parameters bind for its candidates

`proto token p($*K) {*}` now keeps its signature (`Stmt::ProtoToken` carries `param_defs`, the
declaration plan pools it, and the registry records it as `proto_token_params`). When a subrule
call is set up, `subrule_dynamic_params` reads the dynamic parameters from the proto's signature
first and falls back to the candidates' signatures as before, so
`<p('k')>` binds `$*K` around every `p:sym<...>` candidate (and a default such as
`$*K = 'dflt'` applies). Both regex engines share the install path, so both are fixed.
Closes #11071.
