# A nested element store on an expression root autovivifies

A multi-level element store now autovivifies its inner levels whatever its
root is. Rakudo prints `{a => {b => 1}}` for `my %h; (%h)<a><b> = 1; say %h`,
and so does mutsu now; it used to print `{}`. The same holds for
`(@a)[0][1] = 1`, for a call returning a Hash (`store()<k><v> = 7`), and for
`%OUTER::h<a><b> = 1` past a shadowing `my %h` (#10900).

With a variable root, the store runs through the autovivifying
`IndexAssign*Nested` ops by name. With any other root, the inner levels were
read as rvalues (`Index`), which yields Nil for a missing key, so the final
store landed in a throwaway. The compiler already had the fix for one such
root, an accessor call (`$o.h<a><b> = v`): evaluate the root once into a
compiler temp and run the chain against the temp. The container is
reference-shared, so the temp's chain walk vivifies and stores in place. That
rewrite now covers every root that no by-name store reaches, and a
parenthesized variable is simply subscripted as the variable. A type error
raised against the temp names the variable as written (`@a[0]`), not the temp
or its `OUTER::` spelling.
