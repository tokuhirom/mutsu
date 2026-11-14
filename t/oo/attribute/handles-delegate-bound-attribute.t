use Test;

# A `handles` delegation still reaches the delegate's accessors after a
# method of the class bound the attribute (`my $r := $!raw`), which turns
# the attribute into a Scalar cell. Expected values from rakudo. Reduced from
# Font::FreeType (PDF::Font::Loader::CSS): `Face.index-to-unicode` binds
# `my FT_Face $struct := $!raw`, and a later `$face.num-glyphs` (delegated
# through `has FT_Face $.raw handles <num-glyphs ...>`) died with
# "No such method 'num-glyphs'".

plan 7;

class Raw {
    has $.n;
    has $.count is rw = 0;
    method m { 'm' }
    method bump { $!count++ }
}
class Outer {
    has Raw $.raw handles <n m bump count>;
    method bind-it { my $r := $!raw; $r.n }
    method via-self { self.n }
}

my $o = Outer.new(raw => Raw.new(n => 5));
is $o.n, 5, 'delegated accessor before the bind';
is $o.bind-it, 5, 'the attribute is bound in a method';
is $o.n, 5, 'delegated accessor after the bind';
is $o.via-self, 5, '...also through self';
is $o.m, 'm', 'delegated method after the bind';

$o.bump;
$o.bump;
is $o.count, 2, 'a mutating delegated method updates the bound delegate';
is $o.raw.count, 2, '...which the attribute still holds';
