use v6;
use lib 't/lib';
use Test;

# A slang `package-declarator` candidate that parses its own body (ADR-17741):
# `wordlist NAME { words }` reads its body with a second, named language
# (`FOREIGN-LANG('WordList', 'TOP')`), whose actions mint the class, and the
# production's own action binds that class against the resolver `$*R` the way a
# package declaration does: under the enclosing package (`GLOBAL::` escapes to
# the top), with stub packages for intermediate name parts and a lexical for a
# single-part name or a new first part.
#
# The fixture is the shape Grammar::BNF's `bnf-grammar` has; it runs unchanged
# under rakudo, with the same output.

use WordListSlang;

plan 15;

wordlist Solo { alpha beta };
is-deeply Solo.words, <alpha beta>, 'a single-part name is a lexical of the scope';
is Solo.^name, 'Solo', 'the type the foreign language minted keeps its name';

wordlist A::B { gamma };
is-deeply A::B.words, ('gamma',), 'a qualified name binds under a stub package';
is A.^name, 'A', 'the stub package is named after its part';

wordlist A::C { delta };
ok A::B.words && A::C.words, 'a second declaration reuses the stub package';

class K { method kind { 'class' } }
wordlist K::Inner { epsilon zeta };
is-deeply K::Inner.words, <epsilon zeta>, 'a name under an existing class binds into its stash';
is K.kind, 'class', 'the existing class is not replaced by a stub';

wordlist Deep::Down::G { eta };
is Deep::Down.^name, 'Deep::Down', 'an intermediate stub is named with the accumulated parts';
is-deeply Deep::Down::G.words, ('eta',), 'a three-part name resolves through the stubs';

module M {
    wordlist Inner { theta };
    wordlist Nested::In { iota };
    wordlist GLOBAL::Escapee { kappa };
    our $inner = Inner.words;
}
is-deeply M::Inner.words, ('theta',), 'a declaration in a module binds under the module';
is-deeply $M::inner, ('theta',), 'and is a lexical inside it';
nok GLOBAL::<Inner>:exists, 'it stays out of GLOBAL';
is-deeply M::Nested::In.words, ('iota',), 'a multi-part name in a module binds under the module';
is-deeply ::('Escapee').words, ('kappa',), 'a leading GLOBAL part installs at the top level';

sub twice { (^2).map: { wordlist Looped { lambda }; Looped.words } }
is-deeply twice().List, (('lambda',), ('lambda',)), 'a declaration in a routine body runs its action once';

# vim: expandtab shiftwidth=4
