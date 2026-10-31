# Hash shapes and native metaobjects for `static`

For `static` 0.0.1 (#15610), measured against rakudo 2026.09:
`VarDeclaration::Simple.return-type` / `.container-type` of a shaped hash carry
the key type (`my Int %h{Str}` is `Associative[Int,Str]` / `Hash[Int,Str]`,
`my %h{Int}` is `Hash[Any,Int]`), and the native types (`int8`, `uint`, `num32`,
`str`, ...) report `Perl6::Metamodel::NativeHOW`, so the analysis wraps a native
integer to its width. `t/08-constraints` goes from 24 to 0 `not ok` (rakudo: 0).
