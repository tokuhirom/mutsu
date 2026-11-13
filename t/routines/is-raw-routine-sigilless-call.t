use Test;

plan 11;

# An `is raw` routine hands back the container its body produced (#17337:
# such calls now take the positional-light path).

my sub id(\value) is raw { value }

my $a = 1;
id($a) = 5;
is $a, 5, 'assigning to the result writes the caller variable';

my sub idp($x is raw) is raw { $x }
my $b = 1;
idp($b) = 7;
is $b, 7, 'an `is raw` parameter returned raw is the caller container';

my sub plain(\value) { value }
my $c = 1;
throws-like { plain($c) = 9 }, X::Assignment::RO,
  'a routine without `is raw` returns a value';
is $c, 1, 'and the caller variable is untouched';

is id(42), 42, 'a literal argument comes back';

my @arr = 1, 2, 3;
id(@arr[1]) = 20;
is @arr, '1 20 3', 'an array element container comes back';

my %h = a => 1;
id(%h<a>) = 10;
is %h<a>, 10, 'a hash element container comes back';

my sub first-raw(@l) is raw { @l[0] }
first-raw(@arr) = 100;
is @arr, '100 20 3', 'an element read inside the body comes back as its container';

is id(Int).^name, 'Int', 'a type object comes back';

my $r := id($a);
$r = 3;
is $a, 3, 'binding the result binds the caller container';

my $sum = 0;
$sum += id($_) for ^1000;
is $sum, 499500, 'many calls in a loop';
