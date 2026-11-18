# Pod formatting codes take `«…»` and multi-angle delimiters

A Pod formatting code may be delimited by `«…»` or by any run of angles
(`C<<…>>`, `C<<<…>>>`), closing at the first matching run. mutsu recognised
only `<…>` and `<<…>>`. A paragraph or heading whose only code used `«…»` was
not even scanned for codes: `=head2 postcircumfix C«( )»` came out as the
plain text `postcircumfix C«( )»`.

rakudoc's index is built from such headings (`find-definitions` over
`Pod::Heading`s), so its `t/05-index.t` listed `C«( )»` and `C«..»` where
raku lists `( )` and `..`. Its entries now match raku's, apart from the order
and count that rakudo's own `NEXT`-in-a-recursive-loop behaviour produces (see
the PR).
