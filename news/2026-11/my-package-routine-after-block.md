# A module routine reaches its block's `my package` after the block ends

`module M { my package P { our sub f { 42 } }; our sub inner { P::f() } };
M::inner()` died with "Could not find symbol '&f' in 'GLOBAL::P'". A
`my package` is stored under a mangled name, and the qualified call found it
only through the block's lexical binding of `P`, which is gone once the block
has run. The qualified-call fallback now also consults the record of packages
the running package's block declared (added for #18481) and calls the routine
under the package's storage name, as on rakudo (#18520).
