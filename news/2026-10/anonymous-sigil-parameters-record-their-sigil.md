# An anonymous `$` / `@` / `%` parameter records its sigil

`ParamTarget::Anonymous(sigil)` is derived where a parameter is built, and the RakuAST conversion reads
it instead of recognizing the placeholder names `__ANON_STATE#`, `@__ANON_ARRAY__` and
`%__ANON_HASH__`. `make_param` in the signature parser delegates to `ParamDef::blank` rather than
repeating the field list. ADR-10723 section 6.115.
