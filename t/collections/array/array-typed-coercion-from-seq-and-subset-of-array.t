use v6;
use Test;

plan 5;

# `Array[T]()` as a parameter or a `subset ... of Array[T]` return coercion
# builds the typed array from a Seq (#15187, Physics::Unit's `--> Names()`).
subset Name of Str;
subset Names of Array[Name];

sub plain(Array[Str]() $x) { $x.raku }
is plain(<a b>.Seq), 'Array[Str].new("a", "b")', 'Array[Str]() parameter takes a Seq';

sub sub-param(Array[Name]() $x) { $x.raku }
is sub-param(<a b>.Seq), 'Array[Name].new("a", "b")', 'Array[Name]() parameter takes a Seq';

class Dx {
    has %.to-defn{Name};
    method names( --> Names() ) { %.to-defn.keys.sort }
}
my $d = Dx.new;
$d.to-defn<foo> = "bar";
is $d.names.raku, 'Array[Name].new("foo")', '--> Names() coerces a Seq into the subset of an Array type';
ok $d.names ~~ Names, 'the result satisfies the subset';

sub ret( --> Names() ) { <a b>.Seq }
is ret().raku, 'Array[Name].new("a", "b")', 'a routine returning a Seq as Names()';
