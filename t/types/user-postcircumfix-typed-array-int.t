use Test;

plan 8;

# A user `postcircumfix:<[ ]>` candidate whose invocant is a typed array
# (`S @a`) intercepts an Int subscript of such an array, as it does any other
# index (Dan: `df[*][1]` turns a row slice into a column Series).

class S { has $.n }
multi postcircumfix:<[ ]>(S @a, Int $p) { "series $p of {@a.elems}" }

my S @z = S.new, S.new;
is @z[1], 'series 1 of 2', 'a typed array variable';

my $r = @z;
is $r[1], 'series 1 of 2', 'through a scalar';

sub g() { my S @ = S.new xx 3 }
is g()[1], 'series 1 of 3', 'a typed array returned from a sub';

class F { has $.e = 3 }
sub mk(@s) { my S @ = @s.map({ S.new(n => $_) }) }
multi postcircumfix:<[ ]>(F:D $f, Whatever) { mk(0..^$f.e) }
multi postcircumfix:<[ ]>(F:D $f, @s where Range|List) { mk(@s) }
my $f = F.new;
is $f[*][1], 'series 1 of 3', 'chained after a user subscript';
is $f[0..1][1], 'series 1 of 2', 'after a range subscript';

my Int @i = 10, 20, 30;
is @i[1], 20, 'a typed array of another type keeps the built-in subscript';
my @u = <a b c>;
is @u[2], 'c', 'an untyped array keeps the built-in subscript';
is @z[0..1].elems, 2, 'a slice of the typed array is still the built-in one';
