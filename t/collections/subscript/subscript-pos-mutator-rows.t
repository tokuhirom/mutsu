use Test;

# ASSIGN-POS / DELETE-POS on an Array are rows of the method table
# (ADR-11276 §9.37): one handler for a named array, a by-value receiver and the
# backing storage behind a user subclass.

plan 19;

my @a = 1, 2, 3;
my @alias := @a;
is @a.ASSIGN-POS(1, 20), 20, 'ASSIGN-POS answers the value';
is-deeply @alias.List, (1, 20, 3), 'an alias sees the store';
@a.ASSIGN-POS(5, 6);
is @a.elems, 6, 'a store past the end grows the array';
nok @a[4].defined, 'the gap is a hole';
nok @a.EXISTS-POS(4), 'EXISTS-POS agrees';

is @a.DELETE-POS(5), 6, 'DELETE-POS answers the old value';
is @a.elems, 3, 'deleting the last element trims the trailing holes';
is @a.DELETE-POS(0), 1, 'DELETE-POS in the middle';
nok @a.EXISTS-POS(0), 'it leaves a hole';

# --- negative index
dies-ok { @a.ASSIGN-POS(-1, 1) }, 'ASSIGN-POS refuses a negative index';
dies-ok { @a.DELETE-POS(-1) }, 'DELETE-POS refuses a negative index';

# --- typed array
my Int @t;
@t.ASSIGN-POS(0, 5);
is @t[0], 5, 'typed ASSIGN-POS';
throws-like { @t.ASSIGN-POS(1, 'x') }, X::TypeCheck::Assignment, 'the element type is checked';

# --- by-value receiver
sub mk { @a }
is mk().ASSIGN-POS(2, 9), 9, 'a by-value receiver';
is @a[2], 9, 'the shared node saw the store';

# --- shaped array needs every dimension
my @s[2;2];
throws-like { @s.ASSIGN-POS(1, 1) }, X::NotEnoughDimensions, 'a shaped array needs one index per dimension';
@s.ASSIGN-POS(0, 1, 5);
is @s[0;1], 5, 'the multi-dimension form still works';

# --- the backing storage behind a user subclass
class Rounded is Array {
    method ASSIGN-POS($i, $v) { nextwith $i, $v.round }
}
my @r is Rounded;
@r.ASSIGN-POS(0, 2.6);
is @r[0], 3, 'nextwith reaches the Array row on the backing storage';
is @r.DELETE-POS(0), 3, 'DELETE-POS on the storage';

# vim: expandtab shiftwidth=4
