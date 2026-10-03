use v6;
use Test;

# `:delete(COND)` next to `:exists` deletes only when COND holds, whichever
# order the two adverbs are written in. The `:delete(…):exists` order used to
# delete unconditionally.

plan 8;

my %h = a => 1, b => 2;
is %h<a>:delete(0):exists, True, ':delete(0):exists answers the existence';
ok %h<a>:exists, '... and does not delete';
is %h<a>:delete(1):exists, True, ':delete(1):exists answers the existence';
nok %h<a>:exists, '... and deletes';

my $no = False;
is %h<b>:exists:delete($no), True, ':exists:delete($no)';
ok %h<b>:exists, '... does not delete';

my @a = 1, 2, 3;
is @a[0]:delete($no):!exists, False, ':delete($no):!exists on an array';
is @a[0], 1, '... keeps the element';
