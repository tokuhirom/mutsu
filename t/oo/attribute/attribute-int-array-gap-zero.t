use Test;

# An `@` attribute of a native element type (`has uint32 @!to`) is an
# `array[uint32]`, as `my uint32 @a` is: a gap an out-of-range store grows
# reads 0, and the array binds to another native-typed `@` attribute.
# Expected values from rakudo. Reduced from Font::FreeType /
# PDF::Font::Loader (`@!unicode-index := $!face.index-to-unicode`).

plan 9;

class F {
    has uint32 @!to;
    method get { @!to[5] = 0; @!to[2] = 7; @!to }
}
class G {
    has uint32 @.idx;
    method set(\x) { @!idx := x; self }
}
class X {
    has int @.c;
    has num @.e;
    has Int @.d;
}

my $got = F.new.get;
is-deeply $got.List, (0, 0, 7, 0, 0, 0), 'gaps in a native attribute array read 0';
is $got.^name, 'array[uint32]', 'the attribute is a native array';
is-deeply G.new.set($got).idx.List, (0, 0, 7, 0, 0, 0),
    'it binds to another native-typed attribute';

is X.new.c.^name, 'array[int]', 'a default-built native attribute array';
is X.new.d.^name, 'Array[Int]', 'a boxed element type stays an Array[Int]';

my $x = X.new(c => (1, 2), d => (3,));
is $x.c.^name, 'array[int]', 'a constructor-supplied native attribute array';
$x.c[4] = 9;
is-deeply $x.c.List, (1, 2, 0, 0, 9), 'a store through the accessor zero-fills the gap';
$x.d[2] = 1;
is $x.d[2], 1, 'a store through the accessor of a boxed array';
$x.e[1] = 1e0;
is-deeply $x.e.List, (0e0, 1e0), 'a num array gap reads 0e0';
