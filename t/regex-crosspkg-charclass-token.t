use Test;

# When a regex in one package invokes a *qualified* grammar subrule from another
# package (`<Other::Grammar::tok>`), nested *unqualified* token references
# inside that subrule's body — especially char-class `<+name>` items — must
# resolve against the grammar's own package, not the caller's. Previously the
# subrule body was parsed in the caller's package, so the nested name failed to
# resolve ("No such method 'name' for invocant of type 'Match'").

plan 3;

grammar Lib::G {
    token frag       { <+unenc +sub>+ }
    token unenc      { <[:@] +alpha +digit> }
    token sub        { <[;!$&]> }
}

# A different package matching the qualified subrule.
class App::User {
    method check(Str $s) {
        so $s ~~ / ^ <Lib::G::frag> $ /;
    }
}

ok App::User.check('abc'),  'cross-package qualified subrule resolves nested char-class token (letters)';
ok App::User.check('a1:b'), 'cross-package nested char-class accepts digits and bracket members';
nok App::User.check('a b'), 'cross-package nested char-class rejects a space';
