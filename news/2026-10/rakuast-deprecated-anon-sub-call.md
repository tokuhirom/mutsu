# RakuAST: `is DEPRECATED` with an anonymous-sub-call message

Under `MUTSU_RAKUAST=1`, `sub f is DEPRECATED((sub { "a" })()) { }` no longer
refuses the compilation unit. The RakuAST lowering folds a message argument that
statically evaluates to a string (a literal, or an immediately invoked
parameterless anonymous sub whose body is one such expression) and still refuses
anything that would need declaration-time evaluation. The deprecation report now
says `Please use a instead.`, as rakudo does, and `roast/S02-types/isDEPRECATED.t`
passes under the RakuAST frontend. The ordinary frontend still stringifies the
argument source for such messages; true declaration-time evaluation is tracked
separately.
