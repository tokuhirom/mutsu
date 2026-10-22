use v6.d;
use Test;

plan 8;

# An undefined scalar captured by a sub is vivified through the element
# subscript or the element push of that sub (#14637).
my $lookup;
my sub d($node) { $lookup{$node}.push(1); }
d("a");
is $lookup.raku, '${:a($[1])}', 'push on an element of an undefined captured scalar vivifies it';

my $h;
my sub store($k) { $h{$k} = 1 }
store("x");
is $h.raku, '${:x(1)}', 'a keyed store vivifies a Hash';

my $q;
my sub slot { $q<k> = 2; $q.raku }
is slot(), '${:k(2)}', 'the sub sees its own store';
is $q.raku, '${:k(2)}', '... and so does the declaring scope';

my $a;
my sub pos { $a[2] = 7 }
pos();
is $a.raku, '$[Any, Any, 7]', 'a positional store vivifies an Array';

my $e;
my $anon = sub ($n) { $e{$n}.append(5) };
$anon("z");
is $e.raku, '${:z($[5])}', 'an anonymous sub works too';

sub outer { my $f; my sub inner { $f<k> = 2 }; inner(); $f.raku }
is outer(), '${:k(2)}', 'a scalar of a routine captured by its inner sub';

my $defined = {};
my sub add { $defined<n> = 1 }
add();
is $defined.raku, '${:n(1)}', 'an already defined scalar still works';
