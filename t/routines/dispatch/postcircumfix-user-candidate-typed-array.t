use Test;

# A user `postcircumfix:<[ ]>` / `<{ }>` candidate whose first parameter is a
# typed array (`Int @a`, `Foo @rows`) answers a plain element read
# of such an array, as rakudo dispatches it; other arrays keep the
# CORE subscript. Dan's `DataSlice @aods, Int $p` series selection
# (`df[*][1]`) relies on it.

plan 6;

class Foo { has $.v }
class Frame { }

multi postcircumfix:<[ ]>(Int @a, Int $p) { "typed $p" }
multi postcircumfix:<[ ]>(Foo @rows, Int $p) { "foo row $p" }
multi postcircumfix:<[ ]>(Frame:D $f, Whatever) { my Foo @r = Foo.new(v => 1), Foo.new(v => 2); @r }

my Int @a = 1, 2, 3;
is @a[1], 'typed 1', 'Int @a candidate answers @a[1]';
my $item = @a;
is $item[2], 'typed 2', 'also through a scalar holding the typed array';

my @plain = 10, 20;
is @plain[1], 20, 'an untyped array keeps the CORE subscript';

my Foo @rows = Foo.new(v => 7);
is @rows[0], 'foo row 0', 'a user-class element type selects its candidate';

my $rows = Frame.new[*];
is $rows.^name, 'Array[Foo]', 'the first subscript makes a typed array';
is Frame.new[*][1], 'foo row 1', 'a chained subscript dispatches on the typed result';
