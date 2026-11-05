use Test;

# `push %h{$k}, $x` on a hash with a typed element autovivifies the slot with
# that element type, as the method form does (#16115; it blocked Zef's
# `has Array[Distribution] %!short-name-lookup`).

plan 9;

class Dist { }
class Index {
    has Array[Dist] %!lookup;
    method add(Str $key, Dist $d) { push %!lookup{$key}, $d }
    method count($key) { %!lookup{$key}.elems }
    method type($key) { %!lookup{$key}.^name }
}

my $index = Index.new;
my $d = Dist.new;
$index.add($_, $d) for <a b a>;
is $index.count('a'), 2, 'the sub form pushes into an attribute hash slot';
is $index.count('b'), 1, 'one per key';
is $index.type('a'), 'Array[Dist]', 'the slot is an Array of the element type';

my Array[Int] %h;
push %h<x>, 3;
is %h<x>.^name, 'Array[Int]', 'a typed hash variable';
is %h<x>.raku, 'Array[Int].new(3)', 'with the value';
throws-like { push %h<y>, 'str' }, X::TypeCheck, 'a wrong element type is still refused';

my %plain;
push %plain<a><b>, 1, 2;
is-deeply %plain, {:a({:b([1, 2])})}, 'a nested slot of an untyped hash';
my @a;
push @a[2], 5;
is-deeply @a, [Any, Any, [5]], 'an array slot';
my %g;
unshift %g<k>, 1;
append %g<k>, 2, 3;
prepend %g<k>, 0;
is-deeply %g, {:k([0, 1, 2, 3])}, 'unshift, append and prepend';
