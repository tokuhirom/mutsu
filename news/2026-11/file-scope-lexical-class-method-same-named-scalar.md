# A file-scope lexical class's method tells `$R` from the class `R` too

The #18327 fix covered a `my class R` declared in a block or routine. A
file-scope one still mixed up its own name with a file-scope `my $R` in its
methods: `my $R = 5; my class R { method r { $R ~ R.^name } }` gave `5Int`
where rakudo gives `5R`.

A method now reads its own lexical class by the type-only key wherever the
class is declared. Its declaration-time capture binds that key to the
class, so a method of a module's file-scope class still resolves it when it
is called from another compilation unit (#18673).
