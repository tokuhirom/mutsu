use Test;

plan 5;

# A type name right before a user circumfix's closer is an item of the
# circumfix, not a call of the type with an anonymous sigil argument
# (`Int @` read the closer's `@` as a term).
sub circumfix:<[@ @]>(*@items) { 'list(' ~ @items.map(*.raku).join(',') ~ ')' }
sub circumfix:«<@ @>»(*@items) { 'tuple(' ~ @items.map(*.raku).join(',') ~ ')' }

is [@ 1 @], 'list(1)', 'a number before the closer';
is [@ Int @], 'list(Int)', 'a type name before the closer';
is [@ Int:D, Str @], 'list(Int:D,Str)', 'definite type and a type before the closer';
is <@ Int @>, 'tuple(Int)', 'the <@ @> form';
is [@ Int:D @], 'list(Int:D)', 'a smiley type before the closer';
