use Test;

# `Any.splice` is a proto with no candidates, so an instance of a class that
# does not declare its own `splice` and is not an `Array` gets
# X::Multi::NoMatch, not "No such method" (ADR-11276 §9.68, #12647).
plan 18;

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
no-match { X::AdHoc.new.splice(1) }, 'an X::AdHoc exception instance';
no-match { Exception.new.splice(1) }, 'an Exception instance';
no-match { "abc".encode.splice(1) }, 'a utf8 buffer';
no-match { "abc".encode('utf16').splice(1) }, 'a utf16 buffer';
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
