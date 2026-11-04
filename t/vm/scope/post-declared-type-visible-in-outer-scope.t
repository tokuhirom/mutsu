use Test;

# From the rak and ValueClass distributions: `Illegally post-declared type` is
# for a name nothing declares yet at the use. A declaration visible in an outer
# list (rak's `our class Rak { ... }` stub at file level) is what the use binds
# to, so a `class Rak` declared further down in the routine is not "post".
# A sigilless parameter (`-> \Bla { Bla.new }`) binds the name as a term, which
# a later `class Bla` of the unit does not make illegal either.

plan 4;

use MONKEY-SEE-NO-EVAL;

lives-ok { EVAL 'our class Rak { ... }; sub mk { my $r = Rak.new; class Rak { has $.x }; $r }; mk()' },
    'an outer stub is visible to a use before the inner declaration';
lives-ok { EVAL 'class A {...}; sub f { A.new; { class A { } } }; f()' },
    'also from a nested block';
lives-ok { EVAL 'for 1, 2 -> \Bla { say Bla.Str if False; Bla }; class Bla { }' },
    'a sigilless parameter binds the name as a term';
throws-like { EVAL 'sub f { B.new }; class B { }' }, X::Undeclared::Symbols,
    'nothing visible before the declaration is still illegal';
