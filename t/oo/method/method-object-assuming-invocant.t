# A `Method` object primed with `.assuming($invocant)` binds `self`, as a
# direct call of the object does; so does a direct call with a type-object
# invocant. Found in Math::DistanceFunctions, whose
# `get-distance-function` returns `self.^lookup($name).assuming(self)`.
use Test;

plan 11;

class C {
    has $.v = 5;
    method helper() { 20 }
    method m($x) { self.helper + $x }
    method w($x) { self.WHAT.^name ~ $x }
    method a($x) { self.v ~ $x }
    method get($n) { self.^lookup($n).assuming(self) }
    multi method mm($x) { self.helper ~ $x }
}

role R {
    method helper() { 10 }
    method m($x) { self.helper + $x }
    method get($n) { self.^lookup($n).assuming(self) }
}

is C.^lookup('m')(C, 1), 21, 'direct call, type-object invocant';
is C.^lookup('w')(C, 1), 'C1', 'bare self in a direct call, type-object invocant';
is C.^lookup('a')(C.new, 1), '51', 'direct call, instance invocant';
is C.^lookup('a').assuming(C.new)(2), '52', 'assuming an instance invocant';
is C.^lookup('m').assuming(C)(3), 23, 'assuming a type-object invocant';
is C.^lookup('w').assuming(C)(4), 'C4', 'bare self after assuming';
is C.get('m')(5), 25, 'assuming(self) inside a method';
is R.get('m')(6), 16, 'assuming(self) in a role method, called on the pun';
is C.^lookup('mm').assuming(C)(7), '207', 'assuming on a multi method';
is C.^lookup('m').assuming(C).signature.raku, ':($x, *%_)', 'primed signature drops the invocant';
my &m = C.^lookup('m');
is &m.assuming(C.new)(8), 28, 'through a code variable';
