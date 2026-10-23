# A nested class named like a core namespace owns its compound declarations

Inside a package, `class X { }` followed by `class X::Api { }` declared `X::Api` under the
core `X::` exception namespace instead of under the package's own `X`, so the qualified
spelling `Outer::X::Api` raised `Could not find symbol '&Api' in 'Outer::X'`.
The head of a compound name now resolves to a class the enclosing package declared before
it consults global namespaces. `Net::Ethereum`'s `t/09.t` (`Net::Ethereum::X::JSONAPI::...`)
now matches rakudo.
