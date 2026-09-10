use Test;

plan 8;

# `is` on a sigiled variable names the container itself. A type that cannot
# store the declaration initializer must therefore fail instead of leaving an
# ordinary Array or Hash behind.
dies-ok {
    EVAL q[role PlainContainer7775 {}; my @a is PlainContainer7775 = 1, 2, 3]
}, 'a plain role cannot initialize an @ container';

dies-ok {
    EVAL q[role PlainHash7775 {}; my %h is PlainHash7775 = a => 42]
}, 'a plain role cannot initialize a % container';

# A user Positional container with the documented STORE protocol remains a
# valid target. STORE is optional for element access, but is required when the
# declaration itself supplies initial values.
class PositionalContainer7775 does Positional {
    has @!items;

    method STORE(|capture) {
        @!items = capture.list[0].list;
        self
    }

    method AT-POS($index) { @!items[$index] }
    method ASSIGN-POS($index, $value) { @!items[$index] = $value }
    method EXISTS-POS($index) { @!items[$index].defined }
    method elems() { @!items.elems }
}

my @a is PositionalContainer7775 = 1, 2, 3;
is @a.^name, 'PositionalContainer7775', 'a user Positional class is bound as the container';
is @a[1], 2, 'a user Positional class serves element reads';
@a[1] = 9;
is @a[1], 9, 'a user Positional class serves element writes';

my Int @typed is Array[Int] = 1, 2, 3;
is-deeply @typed.List, (1, 2, 3), 'a parameterized Array container still initializes';

dies-ok {
    EVAL q[class PlainBinding7775 {}; my %h := PlainBinding7775.new]
}, 'binding a plain object to a % variable fails its Associative check';

dies-ok {
    EVAL q[class PlainPositionalBinding7775 {}; my @a := PlainPositionalBinding7775.new]
}, 'binding a plain object to an @ variable fails its Positional check';
