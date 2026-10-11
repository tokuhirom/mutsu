use Test;

# `Any.splice` is a proto with no candidates, so an instance of a class that
# does not declare its own `splice` and is not an `Array` gets
# X::Multi::NoMatch, not "No such method" (ADR-11276 §9.68, #12647).
plan 21;

class Foo {}
class WithSplice { method splice(|) { 'mine' } }
class WithFallback { method FALLBACK($name, |) { "fb:$name" } }
class MyArray is Array {}
class MyArrayOverride is Array { method splice(|) { 'over' } }
class MyHash is Hash {}
class MyBag is BagHash {}
class OnlyMu is Mu {}

sub no-match($code, $msg) {
    throws-like $code, X::Multi::NoMatch, $msg;
}

no-match { Foo.new.splice(0, 1) }, 'a plain class instance';
no-match { Foo.new.splice }, 'with no arguments';
no-match { Blob.new(1, 2).splice(0, 1) }, 'a Blob';
no-match { MyHash.new.splice(1) }, 'an is Hash instance';
no-match { MyBag.new.splice(1) }, 'an is BagHash instance';
no-match { Date.new(2020, 1, 1).splice(1) }, 'a Date';
no-match { WithFallback.new.splice(1) }, 'FALLBACK is not consulted: Any.splice exists';

is WithSplice.new.splice(1), 'mine', 'a user splice wins';
is MyArrayOverride.new(1, 2, 3).splice(1), 'over', 'an is Array override wins';

my $a = MyArray.new(1, 2, 3);
is-deeply $a.splice(1, 1).List, (2,), 'an is Array instance still reaches Array.splice';
is-deeply $a.List, (1, 3), '... and mutates it';

my $b = Buf.new(1, 2, 3);
is-deeply $b.splice(1, 1), Buf.new(2), 'Buf.splice unchanged';

throws-like { OnlyMu.new.splice(1) }, X::Method::NotFound, 'a class rooted at Mu has no splice';

throws-like { 5.splice(1) }, X::Multi::NoMatch, 'an Int still gets NoMatch';

# Wrappers around the receiver are seen through (#12653).
role Tag { }

throws-like { (5 but Tag).splice(1) }, X::Multi::NoMatch,
    message => /'splice(Int+{Tag}:D, Int:D)'/, 'a mixin names its type with the role';
throws-like { ("s" but Tag).splice(1, 1) }, X::Multi::NoMatch, 'a mixin around a Str';

my $v = 5;
throws-like { $v.splice(1) }, X::Multi::NoMatch, 'a scalar variable holding an Int';
sub rw-splice($x is rw) { $x.splice(1) }
throws-like { rw-splice($v) }, X::Multi::NoMatch, 'an is rw parameter';
my $arr = [1, 2, 3];
is-deeply $arr.splice(1, 1).List, (2,), 'a scalar variable holding an Array still splices';
is-deeply $arr.List, (1, 3), '... and mutates it';
my %h = a => 1;
throws-like { %h<a>.splice(1) }, X::Multi::NoMatch, 'a hash element holding an Int';
