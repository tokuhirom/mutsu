use Test;

# `OUTERS::` and `LEXICAL::` see every lexical scope out to the setting, so a
# `&name` key no enclosing scope binds is the core routine of that name
# (#17846). A name the setting lacks is still absent.

plan 8;

sub outers-say { OUTERS::<&say> }
sub lexical-say { LEXICAL::<&say> }
is outers-say().name, 'say', 'OUTERS::<&say> from a sub is the core routine';
is lexical-say().name, 'say', 'LEXICAL::<&say> from a sub is the core routine';
is LEXICAL::<&uc>('x'), 'X', 'LEXICAL::<&uc> is callable';

sub outers-missing { OUTERS::<&no-such-routine-here> }
sub lexical-missing { LEXICAL::<&no-such-routine-here> }
is-deeply outers-missing(), Nil, 'OUTERS:: of a routine nobody declares is Nil';
is-deeply lexical-missing(), Nil, 'LEXICAL:: of a routine nobody declares is Nil';

sub mine { 'mine' }
sub lexical-mine { LEXICAL::<&mine> }
is lexical-mine()(), 'mine', 'LEXICAL:: still finds a routine of the program first';
sub shadow {
    my sub uc($x) { 'lexical' }
    LEXICAL::<&uc>('x')
}
is shadow(), 'lexical', 'a lexical routine shadows the core one';

my $x = 7;
sub outers-scalar { OUTERS::<$x> }
is outers-scalar(), 7, 'OUTERS::<$x> still reads the outer variable';
