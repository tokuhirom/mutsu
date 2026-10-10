# `^`-metamethod state is per type; `$o.^meta{k} = v` assigns through the metamethod

Found with the RedFactory distribution. An anonymous `state` (`method ^model($f) is rw { $ }`)
in a metamethod was shared by every type inheriting it; Rakudo gives each type's HOW its own copy,
so the call now scopes the state by the receiver type. Separately, a statement-level subscript
assignment on a metamethod call (`$o.^data{"x"} = 1`) was rewritten into a call of a plain method
named `data`; it now evaluates the metamethod and assigns into the returned container.

A `my $*x` in a tail-position bare block carrying phasers (`KEEP`/`UNDO`/`LEAVE`/...) no longer
raises `X::Dynamic::Postdeclaration` because of a read of `$*x` earlier in the enclosing routine:
the phaser-scope compile of a routine's tail block now opens its own lexical scope.
