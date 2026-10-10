use Test;

# From Pod::To::Markdown (t/declarator.rakutest): a documented routine's
# parameters are rendered with `Parameter.raku`, which must keep a literal
# default, and its WHEREFORE must know multi-ness and attribute accessors.

plan 6;

sub asdf(Str $a, Str :$b? = 'asdf', Int :$c = 5) { }
my @p = &asdf.signature.params;
is @p[1].raku, 'Str :$b = "asdf"', 'named parameter default in Parameter.raku';
is @p[2].raku, 'Int :$c = 5', 'integer default in Parameter.raku';
is @p[0].raku, 'Str $a', 'no default, no suffix';

#| A class
class Doc {
    #| public attr
    has Str $.a = 'x';
    #| private attr
    has Str $!b;
    #| a multi
    multi method m(Str :$x) { }
}

my @w = $=pod.map(*.WHEREFORE);
my $attr = @w.first(Attribute);
ok $attr.has_accessor, 'public attribute WHEREFORE has_accessor';
my $m = @w.first(Method);
ok $m.multi, 'multi method WHEREFORE is multi';
is @w.grep(Attribute).elems, 2, 'both attributes documented';
