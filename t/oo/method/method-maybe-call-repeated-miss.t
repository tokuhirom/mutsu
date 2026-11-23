use Test;

plan 12;

# A repeated argument-less `.?missing` on a user-class instance answers Nil
# from a recorded miss once the first call has proven it (#17987). Whatever
# can make the method appear, or the call answer something else, must still
# be seen on every call.

class A { has $.x; method a { 'a' } }
my $o = A.new;
is ($o.?nope for ^5).map({ $_ === Nil }).all.so, True, 'a repeated miss answers Nil';
is $o.?a, 'a', 'an existing method still answers';
is A.new.?nope, Nil, 'a miss on a fresh instance (a call result, not a variable)';

# The method appears later: `augment` changes the class.
use MONKEY-TYPING;
class B { }
my $b = B.new;
$b.?late for ^3;
augment class B { method late { 'here' } }
is $b.?late, 'here', 'a method added by augment is found after recorded misses';

# A method added through the metamodel.
class C { }
my $c = C.new;
$c.?meta for ^3;
C.^add_method('meta', method { 'added' });
is $c.?meta, 'added', 'a method added with ^add_method is found';

# FALLBACK answers every miss, every time.
my $calls = 0;
class D { method FALLBACK($name, *@) { $calls++; "fb-$name" } }
my $d = D.new;
is ($d.?zzz for ^3).join(','), 'fb-zzz,fb-zzz,fb-zzz', 'FALLBACK answers each call';
is $calls, 3, '...and runs each time';

# A role mixed into one instance gives only that instance the method.
role R { method mixed { 'mixed' } }
class E { }
my $e1 = E.new;
$e1.?mixed for ^3;
my $e2 = E.new but R;
is $e2.?mixed, 'mixed', 'a mixin instance answers the mixed-in method';
is $e1.?mixed, Nil, 'the plain instance still misses';

# A method whose body raises X::Method::NotFound for another name is found,
# so its body runs on every call.
my $runs = 0;
class F { method inner { $runs++; self.no-such-thing } }
my $f = F.new;
for ^3 { try { $f.?inner } }
is $runs, 3, 'an existing method whose body misses runs on every call';

# A subclass that adds the method is a different class.
class G { }
class H is G { method only-h { 'h' } }
G.new.?only-h for ^3;
is H.new.?only-h, 'h', 'a subclass declaring the method answers';
is G.new.?only-h, Nil, 'the parent still misses';
