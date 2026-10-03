use v6;
use Test;

# From App::Racoco::Configuration (ecosystem): a role stubbing
# `multi method get(::?CLASS:D: ...) {...}` must be satisfied by the composing
# class instead of being rejected when the role is declared.
plan 4;

role C {
    multi method get(::?CLASS:D: Str() $key) { ... }
    multi method get(::?CLASS:D: Int $key) { self.get($key.Str) }
}
class E does C {
    multi method get(::?CLASS:D: Str() $key) { "got $key" }
}
is E.new.get("a"), "got a", 'class implements the stubbed multi';
is E.new.get(5), "got 5", 'role candidate dispatches to class candidate';

throws-like q:to/CODE/, X::Role::Unimplemented::Multi, 'unimplemented ::?CLASS multi still throws at composition';
    my role R { multi method m(::?CLASS:D: --> ::?CLASS) {...} }
    my class Foo does R { }
    CODE

throws-like q:to/CODE/, X::Role::Unimplemented::Multi, 'unimplemented plain multi stub throws';
    my role R { multi method m(Int $x) {...} }
    my class Foo does R { }
    CODE
