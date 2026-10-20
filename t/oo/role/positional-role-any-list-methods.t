use Test;

# `Any`'s `self.list.METHOD` methods answer an instance of a user class,
# whatever built-in role it does (#13819): `Positional`/`Associative` are not
# MRO parents, and `head`/`reverse` read the instance's `list`.

plan 13;

class El does Positional { has $.name }
my $e = El.new(:name<a>);
is $e.list.elems, 1, 'Positional: .list';
is $e.flat.elems, 1, 'Positional: .flat';
is $e.head.name, 'a', 'Positional: .head';
is $e.reverse.elems, 1, 'Positional: .reverse';
is $e.pairs.elems, 1, 'Positional: .pairs';
is-deeply El.^mro, (El, Any, Mu), 'a role is not an MRO parent';

class H does Associative { }
is H.new.keys.elems, 1, 'Associative: .keys';
is H.new.head.^name, 'H', 'Associative: .head';

class P { }
is P.new.head.^name, 'P', 'a plain instance: .head';
is P.new.reverse.elems, 1, 'a plain instance: .reverse';
is P.head.^name, 'P', 'a type object: .head';

class L { method list { (3, 4, 5) } }
is L.new.head, 3, '.head reads a user .list';
is L.new.reverse.List, (5, 4, 3), '.reverse reads a user .list';
