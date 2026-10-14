# A candidate's `where` clause sees `self` during the raw-invocant probe

Once any method in the program declared a raw invocant (`method m(\SELF:) is raw`,
which `use NativeCall` does), every method call on a variable resolved its method
once more to ask whether it binds its invocant raw. That resolution did not pass
the invocant, so a multi candidate whose `where` clause reads `self` or a private
attribute (`multi method parse-property(Str:D $n where (%!coerce{.lc}:exists), ...)`)
recorded an `X::Syntax::NoSelf` that surfaced as the call's failure. `resolve_method`
now takes the invocant, and every caller passes it. CSS::Module's
`parse-property` works (#13256).
