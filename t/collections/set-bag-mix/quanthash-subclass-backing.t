use Test;

# A user class that inherits a QuantHash base keeps its entries in a native
# backing store, so the whole Baggy/Setty/Mixy protocol works on it and a
# `my %h is <Subclass>` tie populates it through STORE.

plan 22;

class MyBagHash is BagHash { }
class MyBag     is Bag     { }
class MySetHash is SetHash { }
class MyMixHash is MixHash { }

# --- construction -----------------------------------------------------------
my $empty = MyBagHash.new;
is $empty.^name,  'MyBagHash', 'zero-arg .new keeps the subclass name';
is $empty.elems,  0,           'a fresh QuantHash subclass is empty';
is $empty.total,  0,           '.total answers from the backing store';
ok $empty ~~ Baggy,            'the subclass does Baggy';

my $built = MyBagHash.new('a', 'a', 'b');
is $built.elems, 2,               'positional .new folds its arguments';
is $built.total, 3,               'weights accumulate';
is $built<a>,    2,               'subscript reads the weight';
is $built.gist,  'MyBagHash(a(2) b)', '.gist renders under the subclass name';

# --- mutation ---------------------------------------------------------------
$built<a> = 5;
is $built<a>,    5, 'element assignment sets the weight';
is $built.elems, 2, 'and does not add a key';
$built<b> = 0;
is $built.elems, 1, 'a zero weight removes the element';

my $immutable = MyBag.new('a');
is $immutable.elems, 1, 'an immutable Bag subclass still constructs';
dies-ok { $immutable<a> = 3 }, 'but its elements cannot be assigned';

# --- the mutability of the base is respected --------------------------------
my $set = MySetHash.new('x', 'y');
is $set.elems, 2,    'a SetHash subclass folds to membership';
is $set<x>,    True, 'and reads as a Bool';
$set<z> = True;
is $set.elems, 3, 'adding a member works';

my $mix = MyMixHash.new;
$mix<pi> = 3.14;
is $mix<pi>, 3.14, 'a MixHash subclass keeps a fractional weight';

# --- the tied-variable declaration ------------------------------------------
my %bh is MyBagHash = a => 42, b => 666;
is %bh.^name, 'MyBagHash', 'the tie binds an instance of the subclass';
is %bh<a>,    42,          'the declaration initializer reaches the store';
is %bh<b>,    666,         'for every key';
is %bh.total, 708,         'and nothing else was added';
%bh<a> = 7;
is %bh<a>, 7, 'the tied variable is writable';

# vim: expandtab shiftwidth=4
