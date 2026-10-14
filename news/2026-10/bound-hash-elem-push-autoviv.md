# A variable bound to a missing hash element reads the Array a push autovivifies

`my $t := $h<a>; $t.push: {}` autovivified the Array in the hash but left `$t`
reading as `Any`, so `$t := $t[0]` walked nothing (TOML's array-of-tables
headers lost their keys). A push-family call on such a variable is now a write
through its binding: a shared cell holding the new Array is installed at the
entry and the variable is rebound to it (#13098).
