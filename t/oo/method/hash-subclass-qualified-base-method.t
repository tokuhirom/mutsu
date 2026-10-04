use Test;

# Intl::CLDR: `self.Hash::BIND-KEY(...)` / `$o.Hash::AT-KEY(...)` on a Hash subclass.
plan 4;

class H is Hash { }
my $g = H.new;
$g.Hash::BIND-KEY('b', 2);
is $g<b>, 2, 'Hash::BIND-KEY binds into the subclass';
$g.Hash::ASSIGN-KEY('c', 3);
is $g<c>, 3, 'Hash::ASSIGN-KEY assigns';
is $g.Hash::AT-KEY('b'), 2, 'Hash::AT-KEY reads';
is $g.Hash::elems, 2, 'Hash::elems counts';
