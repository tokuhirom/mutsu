use Test;

plan 6;

# A composing class's own multi candidate replaces a role's candidate only
# when their signatures are the same -- a `where` clause is part of the
# signature, so differently constrained role candidates survive.
role States {
    multi method step(Str $id where $_ ~~ 'start') { 'role start' }
    multi method step(Str $id where $_ ~~ 'stop')  { 'role stop' }
    multi method step(*@rest) { 'fallback' }
}
class Machine does States {
    multi method step(Str $id where $_ ~~ 'stop') { 'class stop' }
}

is Machine.new.step('start'), 'role start', 'role candidate with another where clause survives';
is Machine.new.step('stop'), 'class stop', 'the class candidate wins for its own clause';
is Machine.new.step('other'), 'fallback', 'unmatched input reaches the slurpy candidate';

# Same where clause in role and class: the class's candidate replaces it.
role Same { multi method m(Int $n where * > 0) { 'role' } }
class UsesSame does Same { multi method m(Int $n where * > 0) { 'class' } }
is UsesSame.new.m(1), 'class', 'an identical where clause is still replaced';

# Literal candidates keep working as before.
role Lit { multi method k('a') { 'role a' }; multi method k('b') { 'role b' } }
class UsesLit does Lit { multi method k('b') { 'class b' } }
is UsesLit.new.k('a'), 'role a', 'literal role candidate survives';
is UsesLit.new.k('b'), 'class b', 'literal class candidate replaces its twin';
