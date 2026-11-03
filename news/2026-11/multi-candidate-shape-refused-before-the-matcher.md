# A multi candidate that cannot take a call by shape is refused before the matcher runs

`args_match_multi_candidate_in_scope` entered the candidate's compilation-unit scope and ran the
general `args_match_param_types_inner` (working vectors, an env snapshot, the positional type
checks) for every candidate of a family, including the ones whose SHAPE already ruled them out:
too few or too many positionals, a required named parameter the call did not supply, a named
argument no parameter consumes. A dispatch over `Array::Sorted::Util`'s ~25 `inserts` candidates
paid ~13k Ir for each of ~10 such refusals. Those shape tests (the very ones the matcher makes)
now run first, allocation-free; the named-argument ones only when no `subset` exists and no
parameter has a `where` or sub-signature, so no user code can have run in between (#15786).
