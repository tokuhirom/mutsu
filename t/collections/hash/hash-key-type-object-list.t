use v6;
use Test;

# A bare type object used as a hash key in LIST construction (as opposed to a
# subscript, covered by t/hash-key-type-object.t) coerces to the empty string
# with Rakudo's "uninitialized value of type X in string context" warning. This
# holds across every list-to-hash path: `my %h = (...)`, the `%(...)` literal,
# `.hash`/`.Hash`, the `hash(...)`/`Hash(...)` coercers, and `constant`. A user
# .Str/.Stringy dispatches instead (no warning).

plan 14;

# --- my %h = (...) assignment ---
{
    my %h;
    quietly %h = (Int, 1);
    is %h.keys.raku, '("",).Seq', 'assignment: type object key coerces to ""';
    is %h{""}, 1, 'assignment: the value is stored under ""';
}
{
    my %h;
    quietly %h = (Int, 1, Str, 2);
    is %h.elems, 1, 'assignment: two type object keys collapse to one "" key';
    is %h{""}, 2, 'assignment: the later write wins';
}

# --- %(...) literal ---
{
    my %h = quietly %(Int, 1);
    is %h.keys.raku, '("",).Seq', '%() literal: type object key coerces to ""';
    is %h{""}, 1, '%() literal: value stored under ""';
}

# --- .hash / .Hash methods ---
is (quietly (Int, 1).hash.keys.raku), '("",).Seq', '.hash: type object key coerces to ""';
is (quietly (Int, 1).Hash.keys.raku), '("",).Seq', '.Hash: type object key coerces to ""';

# --- hash(...) / Hash(...) coercers ---
is (quietly hash(Int, 1).keys.raku), '("",).Seq', 'hash(): type object key coerces to ""';
is (quietly Hash(Int, 1).keys.raku), '("",).Seq', 'Hash(): type object key coerces to ""';

# --- a user .Str dispatches (no coercion to "") ---
{
    my class Foo { method Str { "foo" } }
    my %h = (Foo, 1);
    is %h.keys.raku, '("foo",).Seq', 'assignment: user .Str type object keys by its .Str';
    is (Foo, 1).hash.keys.raku, '("foo",).Seq', '.hash: user .Str type object keys by its .Str';
}

# --- ordinary (non-type-object) keys are unaffected ---
{
    my %h = ("a", 1, "b", 2);
    is %h.keys.sort.raku, '("a", "b").Seq', 'plain string keys unaffected';
    my %n = (5, "x");
    is %n.keys.raku, '("5",).Seq', 'numeric key stringifies to "5" as before';
}

done-testing;
