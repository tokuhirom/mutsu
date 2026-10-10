# A class-body `my regex` keeps the body's lexicals when used from another file

`class C { my $e = 'P|D'; my regex fe { <$e> } }` exported a sub using `.subst(&fe, ...)`
interpolated `$e` as empty once called from the importing compunit, so nothing matched
(#11292). The declaration now snapshots the lexicals its pattern interpolates when the class
body registers it, and a `&fe` reference hands that scope to `.subst`/`.match`/`.split`.
