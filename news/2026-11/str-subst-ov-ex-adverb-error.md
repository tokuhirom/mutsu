# `Str.subst` refuses `:ov` and `:ex`

A substitution cannot overlap its matches, so rakudo's `Str.subst` throws
`X::Str::Subst::Adverb` ("Cannot use :ov adverb in Str.subst, got True") for a
true `:ov` or `:ex`. mutsu ignored the adverb and substituted the first match;
it now throws the same exception, with its `name` and `got` (#18897).
