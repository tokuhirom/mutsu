use Test;

# From DB::Migration::Declare t/model-check.rakutest: a `Str @` array handed to
# an untyped `has @.x` must land as a plain Array, so `eqv ['a']` holds.
plan 5;

class Typed { has Str @.from; }
class Plain { has @.columns; has @.d = Typed.new(:from<a b>).from; }

my $t = Typed.new(:from(<a b>));
is $t.from.WHAT.raku, 'Array[Str]', 'typed attribute keeps its element type';

my $p = Plain.new(:columns($t.from));
is $p.columns.WHAT.raku, 'Array', 'untyped attribute stores a plain Array';
ok $p.columns eqv ['a', 'b'], 'eqv against a plain array literal';
is $p.d.WHAT.raku, 'Array', 'default value is a plain Array as well';
is $t.from.WHAT.raku, 'Array[Str]', 'source keeps its type';
