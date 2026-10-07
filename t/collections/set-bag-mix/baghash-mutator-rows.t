use Test;

# `BagHash.add` / `.remove` are rows of the one method table (ADR-11276 §9.23):
# a `Handler::Mut` row reached through `invoke_mut` with the receiver's place.
# Every receiver shape the three former call sites served is pinned here.

plan 21;

my $b = BagHash.new(1, 2, 2);
is $b.add(3).raku, 'Nil', 'add answers Nil';
is $b.sort.gist, '(1 => 1 2 => 2 3 => 1)', 'add one element';

$b.add((4, 5, 5));
is $b.sort.gist, '(1 => 1 2 => 2 3 => 1 4 => 1 5 => 2)',
    'a list argument is iterated one level and a repeated element moves twice';

$b.remove(2);
is $b.sort.gist, '(1 => 1 2 => 1 3 => 1 4 => 1 5 => 2)', 'remove lowers one count';

$b.remove(<absent more>);
is $b.sort.gist, '(1 => 1 2 => 1 3 => 1 4 => 1 5 => 2)',
    'removing an absent key is a no-op and never stores a negative count';

$b.remove(1);
is $b<1>, 0, 'a count at zero drops the key';
nok $b.keys.grep(* eqv 1), 'the key is gone from the keys';

is $b.add(9, :zzz).raku, 'Nil', 'an undeclared named argument is ignored';

# rakudo: `method add(BagHash:D: \to-add, *%_)`
throws-like { $b.add }, X::AdHoc, message => /'Too few positionals passed; expected 2 arguments but got 1'/,
    'no argument is rakudo\'s arity error';
throws-like { $b.add(1, 2) }, X::AdHoc, message => /'Too many positionals passed; expected 2 arguments but got 3'/,
    'two arguments are rakudo\'s arity error';

# a variable declared with the BagHash trait
my %h is BagHash = <x y y>;
%h.add('z');
is %h.sort.gist, '(x => 1 y => 2 z => 1)', 'add on a `%h is BagHash`';

# a second holder of the same container sees the write (container identity)
my $alias = $b;
$alias.add('al');
is $b<al>, 1, 'an alias observes the mutation';

# receivers with no variable name
class Holder { has BagHash $.bag = BagHash.new; }
my $o = Holder.new;
$o.bag.add('q');
$o.bag.add('q');
is $o.bag<q>, 2, 'a call on an attribute accessor';

my @bags;
@bags.push(BagHash.new('a'));
@bags[0].remove('a');
is @bags[0].elems, 0, 'a call on an element';

sub make-bag() { state $s = BagHash.new; $s }
make-bag().add('s');
is make-bag()<s>, 1, 'a call on a function result';

# only `BagHash` declares them
for MixHash.new, SetHash.new, Bag.new, Mix.new -> $x {
    my $name = $x.^name;
    throws-like { $x.add(1) }, X::Method::NotFound, method => 'add', "$name has no add";
}

# a user subclass reaches the same row through its storage
class MyBag is BagHash { }
my $m = MyBag.new(1);
$m.add(2);
is $m.sort.gist, '(1 => 1 2 => 1)', 'add on a user subclass of BagHash';
$m.remove(1);
is $m.sort.gist, '(2 => 1)', 'remove on a user subclass of BagHash';

done-testing;
