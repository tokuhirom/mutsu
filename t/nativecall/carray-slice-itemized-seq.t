use Test;
use NativeCall;

# A Seq whose elements are native element references (a CArray slice) keeps
# the `$` container it is stored in when `.raku` reads it, and is-deeply sees
# the same shape on both sides (#16535). Expectations measured against rakudo.

plan 7;

my $c = CArray[num64].new;
$c[$_] = $_ * 1.5e0 for ^6;

my $s = $c[1, 3];
is $s.raku, '$((1.5e0, 4.5e0).Seq)', 'a stored CArray slice renders itemized';
is-deeply $s, (1.5e0, 4.5e0), 'stored slice is-deeply a List';
is-deeply $c[1, 3], (1.5e0, 4.5e0), 'direct slice is-deeply a List';
is $c[1, 3].raku, '(1.5e0, 4.5e0).Seq', 'an unstored slice renders bare';

my $u = (1, 3).map({ $c.AT-POS($_) });
is $u.raku, '$((1.5e0, 4.5e0).Seq)', 'a stored map over element refs renders itemized';
is $u.elems, 2, 'and still has its elements';

my $t = (1, 3).map({ $c.AT-POS($_) }).List;
is $t.raku, '$(1.5e0, 4.5e0)', 'a stored List over element refs renders itemized';
