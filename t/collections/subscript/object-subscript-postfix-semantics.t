use Test;

class Keyed { method AT-KEY($key) { "key-$key" } }
class Plain { }
my $keyed = Keyed.new;
is $keyed[0].WHICH, $keyed.WHICH, 'positional zero on an ordinary item is the item';
is $keyed{0}, 'key-0', 'associative integer index uses AT-KEY';
is Plain.new<k>.^name, 'Failure', 'unsupported associative indexing returns Failure';
is Plain.new{0}.^name, 'Failure', 'unsupported associative integer indexing returns Failure';
throws-like { Plain.new\i }, X::Multi::NoMatch, 'a plain object has no postfix i candidate';
class Pretender { method Numeric { 3 } }
throws-like { Pretender.new\i }, X::Multi::NoMatch,
    'declaring Numeric alone does not satisfy the postfix i signatures';
class NumericCool is Cool { method Numeric { 4 } }
is NumericCool.new\i, 4i, 'a Cool candidate coerces through Numeric';
class ThrowingCool is Cool { method Numeric { die 'coercion failed' } }
throws-like { ThrowingCool.new\i }, X::AdHoc, message => / 'coercion failed' /,
    'a valid candidate propagates its coercion exception';

class Indexed { method AT-POS($i) { $i ~~ Range ?? 'range' !! "method-$i" } }
multi sub postcircumfix:<[ ]>(Indexed $a, $i) { "custom-$i" }
my $i = Indexed.new;
is-deeply ($i, $i)».[2], ('custom-2', 'custom-2'), 'hyper subscript uses user operator candidates';
is-deeply ($i, $i)».AT-POS(2), ('method-2', 'method-2'), 'explicit hyper method remains a method call';
is-deeply ($i, $i)».AT-POS(0..2), ('range', 'range'), 'explicit hyper accessor receives a range unchanged';
is-deeply [[1, 2], [3, 4]]».[0], (1, 3), 'native hyper subscripts still index each item';
done-testing;
