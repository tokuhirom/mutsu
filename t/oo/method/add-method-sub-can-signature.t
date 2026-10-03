use Test;

# Humming-Bird::Plugin::DBIish installs `sub db(Action $a, Str $database = 'default')`
# with ^add_method and calls `$obj.^can('db')[0].($obj)`.

plan 9;

class A { }
A.^add_method('db', sub db(A $a, Str $database = 'default') { "r:$database" });
A.^add_method('anon', sub ($a, $b = 5) { "anon:$b" });
my $a = A.new;

my $db = $a.^can('db')[0];
is $db.arity, 1, '^can of an added named sub keeps its own arity';
is $db.count, 2, '... and its own count';
is $db.($a), 'r:default', 'named sub is callable with the invocant alone';
is $db.($a, 'x'), 'r:x', '... and with its optional argument';
is $a.db, 'r:default', 'plain method call still works';

my $anon = $a.^can('anon')[0];
is $anon.arity, 1, 'anonymous sub keeps its own arity';
is $anon.($a), 'anon:5', 'anonymous sub callable with the invocant alone';
is $anon.($a, 7), 'anon:7', '... and with its optional argument';
is $a.anon, 'anon:5', 'plain method call still works';
