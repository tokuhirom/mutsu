use Test;

# A `$`-sigil attribute typed `Hash` (`has Hash $.captures`, EEL's continuation
# frames, #16888) constrains the attribute itself, not the hash's elements:
# assigning an element through the accessor stores it. A `%` attribute's type
# still constrains its elements.

plan 6;

class F { has Hash $.captures is required; has Int %.counts }
my %captures;
my $f = F.new(:captures(%captures));
$f.captures<a> = 5;
is $f.captures<a>, 5, 'an element of a Hash-typed scalar attribute is assigned';
is %captures<a>, 5, 'into the hash the attribute holds';
$f.captures<b> = 'text';
is $f.captures<b>, 'text', 'of any type';
$f.captures<n><m> = 1;
is $f.captures<n><m>, 1, 'a nested element autovivifies';

$f.counts<x> = 1;
is $f.counts<x>, 1, 'a typed % attribute takes a matching element';
throws-like { $f.counts<y> = 'no' }, X::TypeCheck, 'and refuses another';
