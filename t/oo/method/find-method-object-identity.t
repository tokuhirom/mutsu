use Test;

# `.^find_method` / `.^lookup` of an existing method hand back the same Method
# object every time (`===`), and the object is rebuilt after anything that
# changes what the name resolves to (#14853).

plan 20;

class C { method foo { 1 }; method bar($x) { $x * 2 } }
my $c = C.new;

my $a = $c.^find_method("foo");
my $b = $c.^find_method("foo");
ok $a === $b, 'two lookups on an instance are the same object';
ok C.^find_method("foo") === $a, 'a type-object lookup is the same object';
ok C.^lookup("foo") === C.^lookup("foo"), '.^lookup is the same object too';
is $a.name, 'foo', 'the name';
is $a.signature.gist, '(C $:: *%_)', 'the signature';
ok $c.^find_method("bar") !=== $a, 'another method is another object';

# A wrap changes the dispatch; the lookup after it still answers, and the
# wrapped call runs.
my $h = $a.wrap(-> | { 10 + callsame });
is $c.foo, 11, 'the wrapped method runs';
ok $c.^find_method("foo").defined, 'the lookup after a wrap answers';
$a.unwrap($h);
is $c.foo, 1, 'unwrapping restores the method';
todo 'rakudo keeps one Method object across a wrap; mutsu rebuilds it when the registry changes (#14853)';
ok $c.^find_method("foo") === $a, 'and the lookup is still the one object';

# A method added afterwards is found; a missing one stays missing.
nok C.^find_method("baz").defined, 'baz is not there yet';
C.^add_method("baz", method { 'baz' });
ok C.^find_method("baz").defined, 'a method added later is found';
is $c.baz, 'baz', 'and callable';
nok C.^find_method("nope").defined, 'a missing method is missing';

# augment adds a method (at compile time in rakudo): it is found.
use MONKEY-TYPING;
augment class C { method qux { 'qux' } }
ok C.^find_method("qux").defined, 'an augmented method is found';

# A subclass overriding a method gets its own object.
class D is C { method foo { 2 } }
ok D.^find_method("foo") !=== C.^find_method("foo"), 'an override is another object';
is D.new.^find_method("foo").package.^name, 'D', 'whose package is the subclass';
todo 'rakudo keeps one Method object across unrelated registry changes (#14853)';
ok C.^find_method("foo") === $a, 'the parent still answers its own object';

# A role composed later is seen by a class that did not have the method.
role R { method rmeth { 'r' } }
class E { }
nok E.^find_method("rmeth").defined, 'no rmeth before composing';
E.^add_role(R);
E.^compose;
ok E.^find_method("rmeth").defined, 'rmeth after composing the role';
