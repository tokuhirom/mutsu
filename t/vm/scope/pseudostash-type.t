use Test;

# Raku keeps a package symbol table (`Stash`) apart from a pseudo-package view
# of a lexical pad (`PseudoStash`). mutsu used to collapse both into `Stash`,
# and the lexical/dynamic spellings that never reached a stash at all answered
# a bare `Hash`.

plan 18;

is MY::.^name,      'PseudoStash', 'MY:: is a PseudoStash';
is OUTER::.^name,   'PseudoStash', 'OUTER:: is a PseudoStash';
is OUTERS::.^name,  'PseudoStash', 'OUTERS:: is a PseudoStash';
is LEXICAL::.^name, 'PseudoStash', 'LEXICAL:: is a PseudoStash';
is DYNAMIC::.^name, 'PseudoStash', 'DYNAMIC:: is a PseudoStash';
is CALLER::.^name,  'PseudoStash', 'CALLER:: is a PseudoStash';
is CALLERS::.^name, 'PseudoStash', 'CALLERS:: is a PseudoStash';
is CORE::.^name,    'PseudoStash', 'CORE:: is a PseudoStash';
is SETTING::.^name, 'PseudoStash', 'SETTING:: is a PseudoStash';
is UNIT::.^name,    'PseudoStash', 'UNIT:: is a PseudoStash';
is CLIENT::.^name,  'PseudoStash', 'CLIENT:: is a PseudoStash';

# The three spellings that name a genuine package symbol table stay Stash.
is OUR::.^name,     'Stash', 'OUR:: is a real package Stash';
is GLOBAL::.^name,  'Stash', 'GLOBAL:: is a real package Stash';
is PROCESS::.^name, 'Stash', 'PROCESS:: is a real package Stash';

sub in-a-sub { CALLER::.^name }
is in-a-sub(), 'PseudoStash', 'CALLER:: taken inside a routine is a PseudoStash';

# PseudoStash is a Map descendant and a *sibling* of Stash, never a subclass:
# implementing it as `class PseudoStash is Stash` would be wrong.
is PseudoStash.^mro.map(*.^name).join(' '), 'PseudoStash Map Cool Any Mu',
    'PseudoStash descends from Map';
nok PseudoStash.^mro.map(*.^name).any eq 'Stash',
    'PseudoStash is not derived from Stash';
is MY::.^mro.map(*.^name).join(' '), 'PseudoStash Map Cool Any Mu',
    'a live pseudo-stash carries that same MRO';
