use Test;

# A store a routine makes by name (here through EVAL) to a variable of its own
# lexical scope is checked against that variable's own type, never against a
# same-named, typed variable of whichever routine called it. The variable's
# type record follows the static link like the variable does (ADR-12529 §3,
# slice 45). Checked against rakudo.

plan 4;

my @a;
sub push-it() { EVAL q[@a.push('s')]; @a.elems }
sub typed-caller() { my Int @a; push-it() }
is typed-caller(), 1, 'a push to the program array ignores a caller my Int @a';

my $x = 1;
sub set-x() { EVAL q[$x = 'str']; $x }
sub int-caller() { my Int $x = 5; set-x() }
is int-caller(), 'str', 'an assignment ignores a caller my Int $x';
is $x, 'str', 'and lands in the program variable';

my Int @typed;
sub push-typed() { EVAL q[@typed.push('s')] }
sub plain-caller() { my @typed; push-typed() }
throws-like { plain-caller() }, X::TypeCheck,
    'the variable\'s own type still applies';
