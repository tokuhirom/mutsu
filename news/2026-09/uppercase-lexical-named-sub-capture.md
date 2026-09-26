# Named subs bind uppercase-named lexicals at their declaration site

A named sub's free variable is bound to the lexical visible where the sub is
declared, through the ADR-0024 capture store. That capture only accepted names
whose first letter is lowercase, because a sigil-less env key cannot tell a
scalar `$F` from a type `F`. An uppercase-named `my` variable therefore fell back
to by-name lookup, and a caller's shadowing `my` of the same name won:

```raku
my $F = 1; sub k { $F = 5 }; { my $F = 9; k() }; say $F;   # was 1, now 5
```

Reads saw the caller's value and writes landed on it, for scalars, arrays,
hashes, sigilless variables and `:=` rebinds alike (#9459). The capture loop
already establishes independently that the key names a slot-backed `my`
variable, so it now uses a case-blind `env::is_user_variable_key`. A variable
spelled like a type (`my $Int`) resolves lexically, and the type is untouched.
The lowercase rule stays in `is_plain_user_lexical`, whose other callers do
need it.

Test: `t/vm/scope/uppercase-mainline-lexical-named-sub.t`.
