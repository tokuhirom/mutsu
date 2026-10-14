use Test;

plan 14;

# A Str-keyed hash coerces a user object key through the key's own .Str.
class K { method Str { "a" } }

my %h = a => 1;
is %h{K.new}, 1, 'subscript read';
is %h.AT-KEY(K.new), 1, 'AT-KEY';
ok %h.EXISTS-KEY(K.new), 'EXISTS-KEY';
ok (%h{K.new}:exists), ':exists';
nok (%h{K.new}:!exists), ':!exists';

my %g;
%g{K.new} = 2;
is %g.keys.join(","), "a", 'subscript store keys by .Str';
%g.ASSIGN-KEY(K.new, 3);
is %g<a>, 3, 'ASSIGN-KEY';
is %g.elems, 1, 'one entry';
is (%g{K.new}:delete), 3, ':delete returns the value';
nok %g.EXISTS-KEY("a"), 'entry deleted';

%g<a> = 5;
%g.DELETE-KEY(K.new);
is %g.elems, 0, 'DELETE-KEY';

# An object hash keeps keying by identity.
my %o{Any};
my $k = K.new;
%o{$k} = 1;
is %o{$k}, 1, 'object hash read by identity';
nok %o{K.new}.defined, 'a different object misses in an object hash';

# A class without its own Str is unchanged.
class P { }
my %p; my $p = P.new;
%p{$p} = 1;
is %p{$p}, 1, 'default-Str object round-trips';
