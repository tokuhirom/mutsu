use Test;

plan 6;

# A Seq default binds an `@` parameter through `.cache`, as a supplied Seq
# does: the parameter holds a List that can be read more than once.
sub f(:@of is raw = (1, 2).Seq) { @of.WHAT }
is f().^name, 'List', 'named `is raw` @-parameter with a Seq default';

sub g(:@of = (1, 2).Seq) { @of.WHAT }
is g().^name, 'List', 'named @-parameter with a Seq default';

sub h(@x is raw = (1, 2).Seq) { @x.WHAT }
is h().^name, 'List', 'positional @-parameter with a Seq default';

sub i(:@of is raw) { @of.WHAT }
is i(of => (1, 2).Seq).^name, 'List', 'a supplied Seq binds as a List too';

sub twice(:@of is raw = (1, 2, 3).map(* + 1)) { (@of.elems, @of.sum) }
is-deeply twice(), (3, 9), 'the default can be iterated twice';

sub again(:@of = (1, 2, 3).map(* + 1)) { @of }
is again().raku, '(2, 3, 4)', 'the bound default reads back its values';
