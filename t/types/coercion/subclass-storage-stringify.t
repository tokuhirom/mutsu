use Test;

# A subclass of `Array`, `Hash` or a QuantHash stringifies its elements in a
# string operator (`~`, `eq`, `lt`, `leg`), as its `.Str` does (#14016).

plan 9;

class A is Array {}
my $a = A.new;
$a.push(1);
$a.push(2);
is $a ~ '!', '1 2!', 'infix ~ on an Array subclass';
ok $a eq '1 2', 'infix eq on an Array subclass';
ok '1 2' eq $a, 'with the subclass on the right';
ok $a eq [1, 2], 'against a plain Array';
ok $a lt '2', 'infix lt';
is $a leg '1 2', Same, 'infix leg';

class H is Hash {}
my %h := H.new;
%h<a> = 1;
is %h ~ '|', "a\t1|", 'infix ~ on a Hash subclass';

class B is Bag {}
ok B.new(<a a>) eq 'a(2)', 'infix eq on a Bag subclass';

class S is Array { method Str { 'mine' } }
is S.new ~ '!', 'mine!', 'a declared Str still wins';
