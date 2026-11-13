use Test;
use nqp;

# The General_Category aggregates (`L`, `P`, ...) are binary properties of
# their own, and a General_Category, Script or Block value name stands for its
# property, as MoarVM's name table has them (Red's regex compiler resolves
# `<:P>` through them, t/109).

plan 15;

is nqp::unipropcode('P'), 97, 'P is a property';
is nqp::unipropcode('Punctuation'), 97, 'and so is its long name';
is nqp::unipropcode('L'), 74, 'L';
is nqp::unipropcode('LC'), 75, 'LC';
is nqp::unipropcode('Lu'), 20, 'a General_Category value stands for General_Category';
is nqp::unipropcode('Greek'), 9, 'a Script value for Script';
is nqp::unipropcode('ASCII'), 6, 'a Block value for Block';
is nqp::getuniprop_int(0x21, 97), 1, '! is punctuation';
is nqp::getuniprop_int(0x41, 97), 0, 'A is not';
is nqp::getuniprop_int(0x1C5, 75), 1, 'a titlecase letter is a cased letter';
is nqp::getuniprop_int(0x378, 38), 0, 'an unassigned codepoint is not in C';
is nqp::getuniprop_int(0xE000, 38), 1, 'a private-use one is';
is nqp::unipvalcode(97, 'P'), 1, 'a binary property names itself as its value';
is nqp::unipvalcode(33, 'alpha'), 1, 'also by a lowercased alias';
is nqp::unipvalcode(97, 'True'), 0, 'but not True';
