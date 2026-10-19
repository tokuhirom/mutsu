# RakuAST preserves combined regex match modifiers

RakuAST now preserves the combined `:ii` and `:ss` adverbs on `m//` expressions
as `ColonPair::True` nodes. Lowering retains the existing case and whitespace
matching behavior, including combinations with `:g`.
