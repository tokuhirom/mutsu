use Test;

# `temp $obj.attr = v` temporizes the ATTRIBUTE, not the variable: the object
# stays the one shared instance, so every other holder of it sees the old value
# again after the scope (#16144, from PDF's `temp $reader.auto-deref = False`).

plan 11;

class R { has Bool $.auto-deref is rw = True; has $.n is rw = 1; }
class H { has $.reader; }

my R $r .= new;
my $h = H.new(:reader($r));
my @a = $r;
my %hv = k => $r;
my $alias = $r;

{
    temp $r.auto-deref = False;
    is $h.reader.auto-deref, False, 'a holder sees the temporized value inside the scope';
}
is $r.auto-deref, True, 'the variable is restored';
is $h.reader.auto-deref, True, 'an object held by another object is restored too';

{
    temp $r.n = 7;
    is-deeply (@a[0].n, %hv<k>.n, $alias.n), (7, 7, 7), 'every holder sees the value inside';
}
is-deeply ($r.n, @a[0].n, %hv<k>.n, $alias.n), (1, 1, 1, 1), 'array, hash and plain aliases are restored';
ok $r === $alias, 'the variable still names the same object';

# a change made inside the scope to ANOTHER attribute survives the restore
{
    temp $r.n = 3;
    $r.auto-deref = False;
}
is-deeply ($r.n, $r.auto-deref), (1, False), 'only the temporized attribute is restored';
$r.auto-deref = True;

# restored on an exception and on a non-local exit
try { temp $r.n = 5; die 'boom' }
is $r.n, 1, 'restored after an exception';

sub early { temp $r.n = 8; return $r.n }
is-deeply (early(), $r.n, $h.reader.n), (8, 1, 1), 'restored after return';

# nested temps restore in reverse order
{
    temp $r.n = 2;
    { temp $r.n = 3; }
    is $r.n, 2, 'the inner temp restores the outer temporized value';
}
is $r.n, 1, 'and the outer one restores the original';
