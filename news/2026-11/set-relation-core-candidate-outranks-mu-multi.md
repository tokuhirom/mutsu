# A user `(Mu, Mu)` set-relation multi no longer beats the core candidate

`multi infix:<⊆>(Mu, Mu)` (Qwiratry's relational operators) took every call,
including `@a ⊆ @b`, where Rakudo's core `(Any, Any)` candidate is narrower.
The four set relations `⊆ ⊇ ⊂ ⊃` (and their ASCII spellings) now have a
modelled core candidate set, so the user candidate has to out-narrow it to take
the call, as `∈`/`∋` already did (#17325).

The same holds for the set operators `∖ ∪ ∩ ⊖ ⊍ ⊎` and their ASCII spellings:
Qwiratry's `(Mu, Mu)` `∖` returned its own list for plain operands where Rakudo
answers the core `Set`. A user candidate declared for a slang's `infix:<⩃>`-style
operator now also reaches the slang's `INFIX-EXPR` action.
