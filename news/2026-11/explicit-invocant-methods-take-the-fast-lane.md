# Methods with a trivially met explicit invocant constraint take the fast lane

A method declared with an explicit invocant constraint (`method m(::?CLASS:D: $x)`,
`Mu \SELF:`, `Base:D:`, `Mu $self:`) was kept off the compiled-method fast lane by a blanket
`has_invocant_constraint` gate, so every call ran the full binder: about 19-24k Ir more than the
same method without the invocant (28k -> 47-52k Ir per call, callgrind slope). A defined instance
receiver against `Mu`, `::?CLASS`, the owner class or its own class (with or without `:D`) is met
by construction, so those calls now take the fast lane; `:U`, `:_`, type captures, coercions,
type-object receivers and any other constraint keep the full binder and its errors (#15833).
