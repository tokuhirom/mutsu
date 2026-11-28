# A typed declaration's Nil gets its type object from the site's constant

After the per-site resolution memo (#12917), a `my Tag $t = $x` or
`my Str $s = $x` whose initializer was Nil still derived the type object to
leave in the variable twice: once in `SetVarTypeScoped`'s seed, by reading
the just-registered constraint back by name and running it through the
nominal-type resolution, and again in the store.

Both now take it from the site's own constant when the site resolved as
written: a plain user class or role, or a plain non-native builtin no env
binding renames. The statement-form declaration's `TypeCheck` (directly
followed by its store) leaves that type object on the stack and vouches for
it, so the store skips its Nil branch; the expression form, whose value is
the initializer's, is unchanged.

Per call (callgrind, profiling build): `my Tag $t = Nil` in a method 20,948 ->
19,007 instructions; `my Str $t = Nil` 19,872 -> 18,077; YAMLish's
two-declaration shape 35,588 -> 30,863. A warm YAMLish `load-yaml` of the
60-row bench document: 103.46M -> 102.69M. Refs #12917.
