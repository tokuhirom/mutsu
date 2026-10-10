# Native `callsame` base bridges take explicit invocant, name and args

The `native_*_base` bridge bodies (`GrammarParse`, `MuBase`, `Metamodel`, `GrammarRule` and the
Array/Hash/QuantHash storage bridges) no longer re-derive the method name, invocant and args from
`samewith_context_stack` / `method_dispatch_stack`, nor re-check the name with a `matches!`. The
advance arm of `dispatch_next_candidate` already holds the `DeferralEntry::Native` name and the
frame's current invocant, args and receiver class, and now passes them in. The one call site with no
frame (a metamodel HOW method with no user MRO frame) goes through a small
`native_metamodel_base_no_frame` wrapper that reads `metamodel_dispatch_stack` and calls the same
explicit-argument body.
