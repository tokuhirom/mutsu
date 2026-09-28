use Test;

# #9823: assigning to a method that a class does not declare at all
# (`$obj.nope = 1`) used to raise X::Multi::NoMatch, "No matching candidates
# for method: nope" -- the diagnostic the runtime falls back to when a
# genuine multi's signatures don't match a call's arguments. But `nope` has
# no overload at all here, so the right exception is the same one a plain
# `$obj.nope` call raises: X::Method::NotFound.

class J {}

throws-like { J.new.destination = 1 }, X::Method::NotFound,
    message => /'No such method \'destination\' for invocant of type \'J\''/,
    'assigning to an undeclared method raises X::Method::NotFound';

throws-like { J.new.destination }, X::Method::NotFound,
    message => /'No such method \'destination\' for invocant of type \'J\''/,
    'and matches the plain (non-assignment) call to the same missing method';

class Point {}

throws-like { my $p = Point.new(x => 1, y => 2); $p.x = 42 }, X::Method::NotFound,
    message => /'No such method \'x\' for invocant of type \'Point\''/,
    'assigning to a name with no declared attribute/accessor is also X::Method::NotFound';

# A genuine multi dispatch failure -- an overload DOES exist, just none of
# its candidates match this call -- must still raise X::Multi::NoMatch.
class C {
    has $.val is rw;
    multi method foo(Int $x) is rw { $!val }
    multi method foo(Str $x) is rw { $!val }
}

throws-like { C.new.foo = 1 }, X::Multi::NoMatch,
    message => /'No matching candidates for method: foo'/,
    'a real multi whose candidates all require an argument still raises X::Multi::NoMatch';

done-testing;
