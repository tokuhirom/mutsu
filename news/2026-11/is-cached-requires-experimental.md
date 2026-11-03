# `is cached` requires `use experimental :cached`

Rakudo refuses the `is cached` routine trait at compile time unless the scope
said `use experimental :cached`; mutsu accepted it everywhere
([#16273](https://github.com/tokuhirom/mutsu/issues/16273), found while fixing
#13346). The parser now keeps a lexical `experimental_cached` flag (set by the
`use`, inherited by nested scopes) and a routine trait `cached` outside it is a
fatal `X::Experimental` with Rakudo's message ("Use of the 'is cached' trait is
experimental; please 'use experimental :cached;'"). No bundled module, vendored
distribution or whitelisted roast file used the trait without the pragma.
Regression test: `t/oo/trait/cached-trait-requires-experimental.t`.
