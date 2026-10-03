use Test;

plan 4;

# `».&f` followed by whitespace and a hyper infix: the infix is not a
# `<<...>>` subscript on the call.
sub f($x) { $x * 2 }
my @a = 1, 2;
is-deeply (@a».&f <<*>> (3, 4)).List, (6, 16), '».&f <<*>> list';
is-deeply (@a».&f <<+>> 1).List, (3, 5), '».&f <<+>> scalar';
is-deeply (@a».&(&f) <<+>> 1).List, (3, 5), '».&(...) <<+>> scalar';
is-deeply @a».&f().List, (2, 4), 'an argument list still attaches';
